{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RecordWildCards
  , ScopedTypeVariables
  , ViewPatterns #-}
module Language.Glyph.Rename
       ( NameException (..)
       , rename
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as Text
import Data.Typeable

import Language.Glyph.Location
import Language.Glyph.Logger
import Language.Glyph.Generics
import Language.Glyph.Ident.Internal
import Language.Glyph.Message
import Language.Glyph.Syntax.Internal

rename :: forall a b m.
         ( Data a
         , HasLocation a
         , MonadLogger Message m
         ) => ([Stmt a], b) -> m ([Stmt a], b)
rename = go
  where
    go (stmts, symtab) = do
      stmts' <- evalStateT' $ rename' stmts
      return (stmts', symtab)
    
    evalStateT' m = evalStateT m initState
    
    rename' stmts = do
      everythingThisScope (>>) (return () `mkQ` defineFunDecl) stmts
      everywhereThisScopeM (mkM transformStmt `extM` transformExpr) stmts
    
    defineFunDecl :: Stmt a -> StateT S m ()
    defineFunDecl x@(view -> FunDeclS name _ _) =
      runReaderT (defineName name) (location x)
    defineFunDecl _ =
      return ()
    
    transformStmt :: Stmt a -> StateT S m (Stmt a)
    transformStmt x@(view -> VarDeclS name _) = do
      runReaderT (defineName name) (location x)
      return x
    transformStmt x@(view -> FunDeclS name params stmts) =
      withScope $ do
        runReaderT (mapM_ defineName params) (location x)
        stmts' <- rename' stmts
        return $ x `updateView` FunDeclS name params stmts'
    transformStmt x@(view -> BlockS stmts) =
      withScope $ do
        stmts' <- rename' stmts
        return $ x `updateView` BlockS stmts'
    transformStmt x =
      return x
    
    transformExpr :: Expr a -> StateT S m (Expr a)
    transformExpr x@(view -> VarE name) = do
      name' <- runReaderT (updateName name) (location x)
      return $ x `updateView` VarE name'
    transformExpr x@(view -> FunE a params stmts) =
      withScope $ do
        runReaderT (mapM_ defineName params) (location x)
        stmts' <- rename' stmts
        return $ x `updateView` FunE a params stmts'
    transformExpr x@(view -> AssignE name expr) = do
      name' <- runReaderT (updateName name) (location x)
      return $ x `updateView` AssignE name' expr
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
  show x =
    case x of
      NotFound a -> "`" ++ Text.unpack a ++ "' not found"
      AlreadyDefined a -> "`" ++ Text.unpack a ++ "' already defined"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception NameException

instance Error NameException where
  strMsg = StrMsgError
  noMsg = NoMsgError

updateName :: ( MonadReader Location m
             , MonadState S m
             , MonadLogger Message m
             ) => Name -> m Name
updateName name = do
  a <- lookupIdent name
  return $ mkName a (view name)

defineName :: ( MonadReader Location m
             , MonadState S m
             , MonadLogger Message m
             ) => Name -> m ()
defineName x = do
  S {..} <- get
  maybe nothing just (Map.lookup (view x) scope)
  where
    nothing =
      insertName x
    just _ =
      logError $ AlreadyDefined $ view x

insertName :: MonadState S m => Name -> m () 
insertName x = do
  s@S {..} <- get
  put s { scope = Map.insert (view x) (ident x) scope }

lookupIdent :: ( MonadReader Location m
              , MonadState S m
              , MonadLogger Message m
              ) => Name -> m Ident
lookupIdent x = do
  S {..} <- get
  maybe nothing just $ lookupIdent' (view x) (scope:scopes)
  where
    nothing = do
      logError $ NotFound (view x)
      return $ ident x
    just =
      return

lookupIdent' :: NameView -> [Map NameView Ident] -> Maybe Ident
lookupIdent' x = go
  where
    go [] =
      Nothing
    go (s:ss) =
      case Map.lookup x s of 
        Nothing -> lookupIdent' x ss
        v@(Just _) -> v

withScope :: MonadState S m => m a -> m a
withScope m = do
  s@S {..} <- get
  put s { scope = mempty, scopes = scope:scopes }
  a <- m
  modify (\ s' -> s' { scope, scopes })
  return a

mkName :: Ident -> NameView -> Name
mkName = Name