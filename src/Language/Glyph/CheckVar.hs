{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RecordWildCards
  , StandaloneDeriving
  , UndecidableInstances
  , ViewPatterns #-}
module Language.Glyph.CheckVar
       ( CheckVarException (..)
       , checkVar
       ) where

import Control.Exception hiding (finally)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text

import Language.Glyph.Generics
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Msg
import Language.Glyph.Syntax

import Text.PrettyPrint.Free

import Prelude hiding (break)

checkVar :: ( Select Name b
            , Select Record.Sort Sort b
            , Select ExtraSet IdentSet b
            , MonadWriter Msgs m
            ) => Record fields -> m ()
checkVar r =
  (runReaderT' R { symtab, afterFinally } .
   runStateT' S { break, scope, scopes } $
   checkStmts before stmts)
  where
    runReaderT' = flip runReaderT
    runStateT' = flip evalStateT
    afterFinally = mempty
    before = mempty
    break = mempty
    scope = mempty
    scopes = mempty

data CheckVarException
  = NotInitialized (Maybe NameView)
  | StrMsgError
  | NoMsgError deriving Typeable

instance Show CheckVarException where
  show = show . pretty

instance Pretty CheckVarException where
  pretty = go
    where
      go (NotInitialized (Just x)) =
        char '`' <> text (Text.unpack x) <> char '\'' </>
        text "may" </> text "not" </>
        text "have" </> text "been" </> 
        text "initialized"
      go (NotInitialized Nothing) =
        text "a" </> text "variable" </> 
        text "may" </> text "not" </> 
        text "have" </> text "been" </> 
        text "initialized"
      go (StrMsgError s) =
        text s
      go NoMsgError =
        text "internal" </>
        text "error"

instance Exception CheckVarException

instance Error CheckVarException where
  strMsg = StrMsgError
  noMsg = NoMsgError

data S
  = S { break :: IdentSet
      , scope :: IdentSet
      , scopes :: [IdentSet]
      }

data R a
  = R { afterFinally :: IdentSet
      , symtab :: IdentMap a
      }

checkStmts :: ( Pretty a
              , Select Name (Just NameView) sym
              , Select Record.Sort Sort sym
              , Select ExtraSet IdentSet sym
              , MonadReader (R sym) m
              , MonadState S m
              , MonadLogger Message m
              ) =>
              IdentSet ->
              [Stmt a] ->
              m (IdentSet, IdentSet, IdentSet)
checkStmts = go
  where
    go before stmts = do
      after <- foldM f before stmts
      S {..} <- get
      let vars = IdentSet.unions (scope : scopes)
      returnExpr $ IdentSet.intersection after vars
    f before stmt = do
      (after, _, _) <- checkStmt before stmt
      return after

checkStmt :: ( Pretty a
             , Select Name (Just NameView) sym
             , Select Record.Sort Sort sym
             , Select ExtraSet IdentSet sym
             , MonadReader (R sym) m
             , MonadState S m
             , MonadLogger Message m
             ) =>
             IdentSet ->
             Stmt a ->
             m (IdentSet, IdentSet, IdentSet)
checkStmt before x = do
  R {..} <- ask
  S { scope, scopes } <- get
  let vars = IdentSet.unions (scope : scopes)
  local (extract x) $ case view x of
    ExprS expr ->
      checkExpr before expr

    VarDeclS (ident -> loc) Nothing -> do
      tellVar loc
      returnExpr before

    VarDeclS (ident -> loc) (Just expr) -> do
      (afterBeta, _, _) <- checkExpr before expr
      tellVar loc
      returnExpr $ IdentSet.insert loc afterBeta

    FunDeclS (ident -> loc) params stmts -> do
      let before' = IdentSet.singleton loc <>
                    IdentSet.fromList (map ident params) <>
                    extraSet (symtab !loc)
      _ <- withScope $ do
        mapM_ (tellVar . ident) params
        checkStmts before' stmts
      returnExpr before

    ReturnS Nothing ->
      returnExpr vars

    ReturnS (Just expr) -> do
      _ <- checkExpr before expr
      returnExpr vars

    IfThenElseS expr trueStmt falseStmt -> do
      (_, true, false) <- checkExpr before expr
      (afterTrue, _, _) <- checkStmt true trueStmt
      (afterFalse, _, _) <- checkMaybeStmt false falseStmt
      returnExpr $ IdentSet.intersection afterTrue afterFalse

    WhileS expr stmt ->
      withLoop $ do
        (_, true, false) <- checkExpr before expr
        _ <- checkStmt true stmt
        break <- getBreak
        returnExpr $ IdentSet.intersection false break

    BreakS -> do
      tellBeforeBreak before
      returnExpr vars

    ContinueS ->
      returnExpr vars

    ThrowS expr -> do
      _ <- checkExpr before expr
      returnExpr vars

    TryFinallyS stmt finally -> do
      (afterFinally', _, _) <- checkMaybeStmt before finally
      (afterTry, _, _) <- withTryCatch afterFinally' $
        checkStmt before stmt
      returnExpr $ afterTry <> afterFinally'

    BlockS stmts ->
      withScope $ checkStmts before stmts

checkExpr :: ( Pretty a
             , Select Name (Just NameView) sym
             , Select Record.Sort Sort sym
             , Select ExtraSet IdentSet sym
             , MonadReader (R sym) m
             , MonadState S m
             , MonadLogger Message m
             ) =>
             IdentSet ->
             Expr a ->
             m (IdentSet, IdentSet, IdentSet)
checkExpr before x = do
  R {..} <- ask
  S {..} <- get
  let vars = IdentSet.unions (scope : scopes)
  withLocation (location x) $ case view x of
    LitE (BoolL True) ->
      returnBool before vars

    LitE (BoolL False) ->
      returnBool vars before

    LitE _lit ->
      returnExpr before

    NotE expr -> do
      (_, true, false) <- checkExpr before expr
      returnBool false true

    VarE loc -> do
      checkBefore before (ident loc)
      returnExpr before

    FunE loc params stmts -> do
      let before' = IdentSet.singleton loc <>
                    IdentSet.fromList (map ident params) <>
                    extraSet (symtab !loc)
      _ <- withScope $ do
        mapM_ (tellVar . ident) params
        checkStmts before' stmts
      checkBefore before loc
      returnExpr before

    ApplyE expr exprs -> do
      let f before' expr' = do
            (after', _, _) <- checkExpr before' expr'
            return after'
      after <- foldM f before (exprs ++ [expr])
      returnExpr after

    AssignE loc expr -> do
      (afterBeta, _, _) <- checkExpr before expr
      returnExpr $ IdentSet.insert (ident loc) afterBeta

checkMaybeStmt :: ( HasName a
                 , HasSort a
                 , HasExtraSet a
                 , HasLocation b
                 , MonadReader (R a) m
                 , MonadState S m
                 , MonadLogger Message m
                 ) =>
                 IdentSet ->
                 Maybe (Stmt b) ->
                 m (IdentSet, IdentSet, IdentSet)
checkMaybeStmt before = maybe (returnExpr before) (checkStmt before)

checkBefore :: ( HasName a
              , HasSort a
              , HasExtraSet a
              , MonadReader (R a sym) m
              , MonadLogger Message m
              ) => IdentSet -> Ident -> m ()
checkBefore before loc = do
  R {..} <- ask
  required <- askBefore loc
  let uninitialized = required \\ before
  unless (IdentSet.null uninitialized) $
    forM_ (IdentSet.toList uninitialized) $
      tell $ Msg.singleton $ mkErrorMsg a $ NotInitialized . Name.name . (symtab !)

askBefore :: ( Select Record.Sort Sort sym
             , Select ExtraSet IdentSet sym
             , MonadReader (R sym) m
             ) => Ident -> m IdentSet
askBefore x = do
  R {..} <- ask
  let sym = symtab ! x
  return $
    case sym#.sort of
      Var -> IdentSet.singleton x
      Fun -> sym#.extraSet

withScope :: MonadState S m => m a -> m a
withScope m = do
  s@S { scope, scopes } <- get
  put s { scope = mempty, scopes = scope : scopes }
  a <- m
  modify (\ s' -> s' { scope, scopes })
  return a

tellVar :: MonadState S m => Ident -> m ()
tellVar x = modify f
  where
    f s@S {..} = s { scope = IdentSet.insert x scope }

withLoop :: (MonadReader (R a) m, MonadState S m) => m b -> m b
withLoop m = do
  R {..} <- ask
  s@S {..} <- get
  let vars = IdentSet.unions (scope : scopes)
  put s { break = vars }
  a <- local (\ r -> r { afterFinally = mempty }) m
  put s
  return a

withTryCatch :: MonadReader (R a) m => IdentSet -> m b -> m b
withTryCatch x = local (\ r@R {..} -> r { afterFinally = afterFinally <> x })

getBreak :: MonadState S m => m IdentSet
getBreak = gets break

tellBeforeBreak :: (MonadReader (R a) m, MonadState S m) => IdentSet -> m ()
tellBeforeBreak beforeBreak = do
  R {..} <- ask
  modify $ \ s@S {..} ->
    s { break = IdentSet.intersection break (beforeBreak <> afterFinally) }

returnBool :: Monad m => IdentSet -> IdentSet -> m (IdentSet, IdentSet, IdentSet)
returnBool true false = return (IdentSet.intersection true false, true, false)

returnExpr :: Monad m => IdentSet -> m (IdentSet, IdentSet, IdentSet)
returnExpr after = return (after, after, after)
