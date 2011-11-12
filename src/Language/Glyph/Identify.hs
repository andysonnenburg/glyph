{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RecordWildCards
  , StandaloneDeriving #-}
module Language.Glyph.Identify
       ( Ident
       , IdentException (..)
       , identify
       ) where

import Control.Applicative
import Control.Comonad
import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.State hiding (get, mapM, modify, put)
import qualified Control.Monad.State as State
import Control.Monad.Writer.Class

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Traversable
import Data.Typeable

import Language.Glyph.Internal.Ident
import Language.Glyph.Location
import Language.Glyph.Message
import Language.Glyph.Syntax

import Prelude hiding (mapM)

identify :: MonadWriter Message m =>
            [Located (Stmt ())] ->
            m [Located (Stmt Ident)]
identify = runIdentT . identifyStmts

newtype IdentT m a
  = IdentT ( StateT S m a
           ) deriving ( Functor
                      , Applicative
                      , Monad
                      )

deriving instance MonadError IdentException m => MonadError IdentException (IdentT m)
deriving instance MonadWriter Message m => MonadWriter Message (IdentT m)

data S
  = S { nameCount :: Int
      , scope :: Scope
      , scopes :: [Scope]
      }

type Scope = Map Name Ident

data IdentException
  = NotFound (Located Name)
  | AlreadyDefined (Located Name)
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show IdentException where
  show x =
    case x of
      NotFound a -> showLocated a ++ " not found"
      AlreadyDefined a -> showLocated a ++ " already defined"
      StrMsgError s -> s
      NoMsgError -> "internal error"
    where
      showLocated a =
        show (location a) ++ ": `" ++ Text.unpack (extract a) ++ "'"

instance Exception IdentException

instance Error IdentException where
  strMsg = StrMsgError
  noMsg = NoMsgError

runIdentT :: Monad m => IdentT m a -> m a
runIdentT (IdentT m) = evalStateT m initState
  where
    initState = S { nameCount = 0
                  , scope = Map.empty
                  , scopes = []
                  }

identifyStmts :: MonadWriter Message m =>
                 [Located (Stmt ())] ->
                 IdentT m [Located (Stmt Ident)]
identifyStmts stmts = do
  mapM_ identifyFunDecl stmts
  notFunDecls' <- mapM identifyStmt notFunDecls
  funDecls' <- mapM identifyStmt funDecls
  return $ notFunDecls' ++ funDecls'
  where
    (funDecls, notFunDecls) = partition (isFunDecl . extract) stmts
    isFunDecl x = case x of (FunDeclS _ _ _) -> True; _ -> False

identifyStmt :: MonadWriter Message m =>
               Located (Stmt ()) ->
               IdentT m (Located (Stmt Ident))
identifyStmt x = do
  x' <- identifyStmt'
  return $ x' <$ x
  where
    identifyStmt' =
      case extract x of
        ExprS expr -> do
          expr' <- identifyExpr expr
          return $ ExprS expr'
    
        VarDeclS named m -> do
          m' <- mapM identifyExpr m
          named' <- defineIdent named
          return $ VarDeclS named' m'
      
        FunDeclS named params stmts -> do
          named' <- lookupIdent named
          withScope $ do
            params' <- mapM defineIdent params
            mapM_ identifyFunDecl stmts
            stmts' <- mapM identifyStmt stmts
            return $ FunDeclS named' params' stmts'
      
        ReturnS m -> do
          m' <- mapM identifyExpr m
          return $ ReturnS m'
      
        IfThenElseS expr stmt m -> do
          expr' <- identifyExpr expr
          stmt' <- identifyStmt stmt
          m' <- mapM identifyStmt m
          return $ IfThenElseS expr' stmt' m'
      
        WhileS expr stmt -> do
          expr' <- identifyExpr expr
          stmt' <- identifyStmt stmt
          return $ WhileS expr' stmt'

        BlockS stmts ->
          withScope $ do
            mapM_ identifyFunDecl stmts
            stmts' <- mapM identifyStmt stmts
            return $ BlockS stmts'

identifyExpr :: MonadWriter Message m =>
               Located (Expr ()) ->
               IdentT m (Located (Expr Ident))
identifyExpr x = do
  x' <- identifyExpr'
  return $ x' <$ x
  where
    identifyExpr' =
      case extract x of
        IntE a ->
          return $ IntE a
      
        BoolE a ->
          return $ BoolE a
      
        VarE named -> do
          named' <- lookupIdent (named <$ x)
          return $ VarE (extract named')
      
        FunE _ params stmts -> do
          a' <- newIdent
          withScope $ do
            params' <- mapM defineIdent params
            mapM_ identifyFunDecl stmts
            stmts' <- mapM identifyStmt stmts
            return $ FunE a' params' stmts'
      
        ObjE props -> do
          props' <- mapM f props
          return $ ObjE props'
          where
            f (s, expr) = do
              expr' <- identifyExpr expr
              return (s, expr')
      
        ApplyE expr exprs -> do
          expr' <- identifyExpr expr
          exprs' <- mapM identifyExpr exprs
          return $ ApplyE expr' exprs'
      
        AssignE named expr -> do
          named' <- lookupIdent named
          expr' <- identifyExpr expr
          return $ AssignE named' expr'

identifyFunDecl :: MonadWriter Message m => Located (Stmt ()) -> IdentT m ()
identifyFunDecl x =
  case extract x of
    FunDeclS a _ _ -> do
      _ <- defineIdent a
      return ()
    _ ->
      return ()

defineIdent :: MonadWriter Message m =>
               Located (Named ()) ->
               IdentT m (Located (Named Ident))
defineIdent x = do
  S {..} <- get
  i <- maybe nothing just (Map.lookup n scope)
  return $ fmap (const i) <$> x
  where
    nothing = do
      i <- newIdent
      insertIdent n i
      return i
    just i = do 
      tell $ Error $ AlreadyDefined $ name <$> x
      return i
    n = name . extract $ x

insertIdent :: Monad m => Name -> Ident -> IdentT m () 
insertIdent t n = do
  s@S {..} <- get
  put s { scope = Map.insert t n scope }

newIdent :: Monad m => IdentT m Ident
newIdent = do
  s@S {..} <- get
  put s { nameCount = nameCount + 1 }
  return $ Ident nameCount

lookupIdent :: MonadWriter Message m =>
               Located (Named a) ->
               IdentT m (Located (Named Ident))
lookupIdent x = do
  S {..} <- get
  n <- maybe nothing just $ lookupIdent' t (scope:scopes)
  return $ fmap (const n) <$> x
  where
    nothing = do
      let e = NotFound $ t <$ x
      tell $ Error e
      return $ Ident $ throw e
    just =
      return
    Named t _ = extract x

lookupIdent' :: Name -> [Map Name Ident] -> Maybe Ident
lookupIdent' _ [] = Nothing
lookupIdent' x (s:ss) =
  case Map.lookup x s of 
    Nothing -> lookupIdent' x ss
    v@(Just _) -> v

withScope :: Monad m => IdentT m a -> IdentT m a
withScope m = do
  s@S { scope, scopes } <- get
  put s { scope = Map.empty, scopes = scope:scopes }
  a <- m
  modify (\ s -> s { scope, scopes })
  return a

get :: Monad m => IdentT m S
get = IdentT State.get

put :: Monad m => S -> IdentT m ()
put = IdentT . State.put

modify :: Monad m => (S -> S) -> IdentT m ()
modify = IdentT . State.modify