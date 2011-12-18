{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , ScopedTypeVariables
  , ViewPatterns #-}
module Language.Glyph.CheckReturn
       ( checkReturn
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Generics

import Language.Glyph.Location
import Language.Glyph.Message
import Language.Glyph.Syntax

checkReturn :: forall a b m.
              ( Data a
              , HasLocation a
              , MonadWriter Message m
              ) => ([Stmt a], b) -> m ([Stmt a], b)
checkReturn (stmts, symtab) = do
  checkReturn' stmts
  return (stmts, symtab)
  where
    checkReturn' = runReaderT' . query
    
    query :: Data c => c -> ReaderT Bool m ()
    query = everythingBut (>>)
            ((return (), False) `mkQ`
             queryStmt `extQ`
             queryStmtView)
    
    queryStmt :: Stmt a -> (ReaderT Bool m (), Bool)
    queryStmt x@(view -> ReturnS _) = (m, True)
      where
        m = do
          illegalReturn <- ask
          when illegalReturn $
            runReaderT (tellError IllegalReturn) (location x)
    queryStmt _ = (return (), False)
    
    queryStmtView :: StmtView a -> (ReaderT Bool m (), Bool)
    queryStmtView (TryFinallyS stmt1 stmt2) = (m, True)
      where
        m = do
          query stmt1
          local (const True) $ query stmt2
    queryStmtView _ = (return (), False)
    
    runReaderT' = flip runReaderT False

data CheckReturnException
  = IllegalReturn
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show CheckReturnException where
  show x =
    case x of
      IllegalReturn -> "illegal return statement"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception CheckReturnException

instance Error CheckReturnException where
  strMsg = StrMsgError
  noMsg = NoMsgError
