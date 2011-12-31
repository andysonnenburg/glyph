{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , ScopedTypeVariables
  , ViewPatterns #-}
module Language.Glyph.CheckContinue
       ( checkContinue
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader

import Data.Generics

import Language.Glyph.Location
import Language.Glyph.Logger
import Language.Glyph.Message
import Language.Glyph.Syntax

checkContinue :: forall a b m .
                ( Data a
                , HasLocation a
                , MonadLogger Message m
                ) => ([Stmt a], b) -> m ([Stmt a], b)
checkContinue (stmts, symtab) = do
  checkContinue' stmts
  return (stmts, symtab)
  where
    checkContinue' = runReaderT' . query

    query :: Data c => c -> ReaderT Bool m ()
    query = everythingBut (>>)
            ((return (), False) `mkQ`
             queryStmt `extQ`
             queryStmtView `extQ`
             queryExprView)

    queryStmt :: Stmt a -> (ReaderT Bool m (), Bool)
    queryStmt x@(view -> ContinueS) = (m, True)
      where
        m = do
          illegalContinue <- ask
          when illegalContinue $
            runReaderT (logError IllegalContinue) (location x)
    queryStmt _ = (return (), False)

    queryStmtView :: StmtView a -> (ReaderT Bool m (), Bool)
    queryStmtView (FunDeclS _ _ stmt) = (m, True)
      where
        m = local (const True) $ query stmt
    queryStmtView (WhileS expr stmt) = (m, True)
      where
        m = do
          query expr
          local (const False) $ query stmt
    queryStmtView (TryFinallyS stmt1 stmt2) = (m, True)
      where
        m = do
          query stmt1
          local (const True) $ query stmt2
    queryStmtView _ = (return (), False)

    queryExprView :: ExprView a -> (ReaderT Bool m (), Bool)
    queryExprView (FunE _ _ stmt) =
      (local (const True) $ query stmt, True)
    queryExprView _ =
      (return (), False)

    runReaderT' = flip runReaderT True

data CheckContinueException
  = IllegalContinue
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show CheckContinueException where
  show x =
    case x of
      IllegalContinue -> "illegal continue statement"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception CheckContinueException

instance Error CheckContinueException where
  strMsg = StrMsgError
  noMsg = NoMsgError
