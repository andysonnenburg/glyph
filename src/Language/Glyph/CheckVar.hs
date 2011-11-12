{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, ViewPatterns #-}
module Language.Glyph.CheckVar
       ( CheckVarException (..)
       , checkVar
       ) where

import Control.Applicative
import Control.Comonad
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Text as Text

import Language.Glyph.Annotation.ExtraSet
import Language.Glyph.Annotation.Sort
import Language.Glyph.Generics
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Location
import Language.Glyph.Message
import Language.Glyph.Syntax

checkVar :: ( HasSort a
            , HasExtraSet a
            , MonadReader [Located (Stmt Ident)] m
            , MonadWriter Message m
            , MonadIO m
            ) => IdentMap a -> m (IdentMap a)
checkVar symtab = do
  ask >>= evalStateT' mempty . checkVarQ symtab
  return symtab
  where
    evalStateT' = flip evalStateT

data CheckVarException
  = NotInitialized (Located Name)
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show CheckVarException where
  show x =
    case x of
      NotInitialized a ->
        show (location a) ++ ": `" ++ Text.unpack (extract a) ++
        "' may not have been initialized"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception CheckVarException

instance Error CheckVarException where
  strMsg = StrMsgError
  noMsg = NoMsgError

checkVarQ :: ( HasSort a
            , HasExtraSet a
            , Data b
            , MonadWriter Message m
            , MonadIO m
            ) => IdentMap a -> b -> StateT IdentSet m ()
checkVarQ symtab =
  everythingButFuns (>>)
  (return () `mkQ` queryStmt `extQ` queryExpr `extQ` queryLocatedExpr)
  where
    queryStmt (FunDeclS x params stmts) =
      queryFun (ident x) params stmts
    queryStmt (VarDeclS x m) | isJust m =
      modify (IdentSet.insert (ident x))
    queryStmt _ =
      return ()

    queryExpr (FunE x params stmts) =
      queryFun x params stmts
    queryExpr (AssignE x expr) = do
      modify (IdentSet.insert (ident x))
    queryExpr _ =
      return ()

    queryLocatedExpr w@(extract -> VarE x) =
      case sort (symtab!k) of
        Var -> do
          initialized <- get
          liftIO $ putStrLn "var"
          when (IdentSet.notMember k initialized) $
            tell $ Error $ NotInitialized n
        Fun -> do
          liftIO $ putStrLn "fun"
          initialized <- get
          let uninitialized = extraSet (symtab!k) \\ initialized
          unless (IdentSet.null uninitialized) $
            tell $ Error $ NotInitialized n
      where
        n = name x <$ w
        k = extract x
    queryLocatedExpr _ =
      return ()

    queryFun x params stmts =
      return ()

    ident = extract . extract
