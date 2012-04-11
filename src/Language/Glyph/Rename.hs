{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RecordWildCards
  , ScopedTypeVariables #-}
module Language.Glyph.Rename
       ( NameException (..)
       , rename
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer hiding ((<>))

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Typeable

import Language.Glyph.Generics
import Language.Glyph.Ident
import Language.Glyph.Msg
import qualified Language.Glyph.Msg as Msg
import Language.Glyph.Syntax

import Text.PrettyPrint.Free

rename :: forall a m .
          ( Data a
          , Pretty a
          , MonadWriter Msgs m
          ) => [Stmt a] -> m [Stmt a]
rename = evalStateT' . rename'
  where
    rename' stmts = do
      everythingThisScope (>>) (return () `mkQ` defineFunDecl) stmts
      everywhereThisScopeM (mkM transformStmt `extM` transformExpr) stmts

    evalStateT' m = evalStateT m initState

    defineFunDecl :: Stmt a -> StateT S m ()
    defineFunDecl (Stmt a (FunDeclS name _ _)) =
      runReaderT (defineName name) a
    defineFunDecl _ =
      return ()

    transformStmt :: Stmt a -> StateT S m (Stmt a)
    transformStmt x@(Stmt a (VarDeclS name _)) = do
      runReaderT (defineName name) a
      return x
    transformStmt (Stmt a (FunDeclS name params stmts)) =
      withScope $ do
        runReaderT (mapM_ defineName params) a
        stmts' <- rename' stmts
        return $ Stmt a $ FunDeclS name params stmts'
    transformStmt (Stmt a (BlockS stmts)) =
      withScope $ do
        stmts' <- rename' stmts
        return $ Stmt a $ BlockS stmts'
    transformStmt x =
      return x

    transformExpr :: Expr a -> StateT S m (Expr a)
    transformExpr (Expr a (VarE name)) = do
      name' <- runReaderT (updateName name) a
      return $ Expr a $ VarE name'
    transformExpr (Expr a (FunE x params stmts)) =
      withScope $ do
        runReaderT (mapM_ defineName params) a
        stmts' <- rename' stmts
        return $ Expr a $ FunE x params stmts'
    transformExpr (Expr a (AssignE name expr)) = do
      name' <- runReaderT (updateName name) a
      return $ Expr a $ AssignE name' expr
    transformExpr x =
      return x

data S
  = S { scope :: Scope
      , scopes :: [Scope]
      }

initState :: S
initState =
  S { scope = mempty
    , scopes = []
    }

type Scope = Map NameView Ident

data NameException
  = NotFound NameView
  | AlreadyDefined NameView
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show NameException where
  show = show . pretty

instance Pretty NameException where
  pretty = go
    where
      go (NotFound a) =
        char '`' <> text (Text.unpack a) <> char '\'' <+>
        text "not" <+> text "found"
      go (AlreadyDefined a) =
        char '`' <> text (Text.unpack a) <> char '\'' <+>
        text "already" <+> text "defined"
      go (StrMsgError s) =
        text s
      go NoMsgError =
        text "internal" <+> text "error"

instance Exception NameException

instance Error NameException where
  strMsg = StrMsgError
  noMsg = NoMsgError

updateName :: ( Pretty a
              , MonadReader a m
              , MonadState S m
              , MonadWriter Msgs m
              ) => Name -> m Name
updateName name = do
  a <- lookupIdent name
  return $ mkName a (view name)

defineName :: ( Pretty a
              , MonadReader a m
              , MonadState S m
              , MonadWriter Msgs m
              ) => Name -> m ()
defineName x = do
  S {..} <- get
  maybe nothing just $ Map.lookup (view x) scope
  where
    nothing =
      insertName x
    just _ = do
      a <- ask
      tell $ Msg.singleton $ mkErrorMsg a $ AlreadyDefined $ view x

insertName :: MonadState S m => Name -> m ()
insertName x = do
  s@S {..} <- get
  put s { scope = Map.insert (view x) (ident x) scope }

lookupIdent :: ( Pretty a
               , MonadReader a m
               , MonadState S m
               , MonadWriter Msgs m
               ) => Name -> m Ident
lookupIdent x = do
  S {..} <- get
  maybe nothing just $ lookupIdent' (view x) (scope : scopes)
  where
    nothing = do
      a <- ask
      tell $ Msg.singleton $ mkErrorMsg a $ NotFound $ view x
      return $ ident x
    just =
      return

lookupIdent' :: NameView -> [Map NameView Ident] -> Maybe Ident
lookupIdent' x = go
  where
    go [] =
      Nothing
    go (s : ss) =
      case Map.lookup x s of
        Nothing -> lookupIdent' x ss
        v@(Just _) -> v

withScope :: MonadState S m => m a -> m a
withScope m = do
  s@S {..} <- get
  put s { scope = mempty, scopes = scope : scopes }
  a <- m
  modify (\ s' -> s' { scope, scopes })
  return a

mkName :: Ident -> NameView -> Name
mkName = Name
