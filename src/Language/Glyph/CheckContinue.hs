{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , ScopedTypeVariables #-}
module Language.Glyph.CheckContinue
       ( checkContinue
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Generics

import Language.Glyph.Msg
import qualified Language.Glyph.Msg as Msg
import Language.Glyph.Syntax

import Text.PrettyPrint.Free

checkContinue :: forall a m .
                 ( Data a
                 , Pretty a
                 , MonadWriter Msgs m
                 ) => [Stmt a] -> m ()
checkContinue = checkContinue'
  where
    checkContinue' = runReaderT' . query

    query :: forall a . Data a => a -> ReaderT Bool m ()
    query =
      everythingBut (>>)
      ((return (), False) `mkQ`
       queryStmt `extQ`
       queryStmtView `extQ`
       queryExprView)

    queryStmt :: Stmt a -> (ReaderT Bool m (), Bool)
    queryStmt (Stmt a ContinueS) = (m, True)
      where
        m = do
          illegalContinue <- ask
          when illegalContinue $
            tell $ Msg.singleton $ mkErrorMsg a IllegalContinue
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
  show = show . pretty

instance Pretty CheckContinueException where
  pretty = go
    where
      go IllegalContinue =
        text "illegal" </>
        text "continue" </>
        text "statement"
      go (StrMsgError s) =
        text s
      go NoMsgError =
        text "internal" </>
        text "error"

instance Exception CheckContinueException

instance Error CheckContinueException where
  strMsg = StrMsgError
  noMsg = NoMsgError
