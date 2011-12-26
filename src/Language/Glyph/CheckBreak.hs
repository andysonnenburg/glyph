{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , ScopedTypeVariables
  , ViewPatterns #-}
module Language.Glyph.CheckBreak
       ( checkBreak
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader

import Data.Generics

import Language.Glyph.Location
import Language.Glyph.Logger
import Language.Glyph.Message
import Language.Glyph.Syntax

checkBreak :: forall a b m.
             ( Data a
             , HasLocation a
             , MonadLogger Message m
             ) => ([Stmt a], b) -> m ([Stmt a], b)
checkBreak (stmts, symtab) = do
  checkBreak' stmts
  return (stmts, symtab)
  where
    checkBreak' = runReaderT' . query
    
    query :: Data c => c -> ReaderT Bool m ()
    query = everythingBut (>>)
            ((return (), False)
             `mkQ` queryStmt
             `extQ` queryStmtView
             `extQ` queryExprView)
    
    queryStmt :: Stmt a -> (ReaderT Bool m (), Bool)
    queryStmt x@(view -> BreakS) = (m, True)
      where
        m = do
          illegalBreak <- ask
          when illegalBreak $
            runReaderT (logError IllegalBreak) (location x)
    queryStmt _ = (return (), False)
    
    queryStmtView :: StmtView a -> (ReaderT Bool m (), Bool)
    queryStmtView (FunDeclS _ _ stmt) =
      (local (const True) $ query stmt, True)
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
    queryStmtView _ =
      (return (), False)
    
    queryExprView :: ExprView a -> (ReaderT Bool m (), Bool)
    queryExprView (FunE _ _ stmt) =
      (local (const True) $ query stmt, True)
    queryExprView _ =
      (return (), False)
    
    runReaderT' = flip runReaderT True

data CheckBreakException
  = IllegalBreak
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show CheckBreakException where
  show x =
    case x of
      IllegalBreak -> "illegal break statement"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception CheckBreakException

instance Error CheckBreakException where
  strMsg = StrMsgError
  noMsg = NoMsgError
