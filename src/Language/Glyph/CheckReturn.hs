{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , ScopedTypeVariables #-}
module Language.Glyph.CheckReturn
       ( checkReturn
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

checkReturn :: forall a m .
               ( Data a
               , Pretty a
               , MonadWriter Msgs m
               ) => [Stmt a] -> m ()
checkReturn = checkReturn'
  where
    checkReturn' = runReaderT' . query

    query :: forall a . Data a => a -> ReaderT Bool m ()
    query =
      everythingBut (>>)
      ((return (), False) `mkQ`
       queryStmt `extQ`
       queryStmtView)

    queryStmt :: Stmt a -> (ReaderT Bool m (), Bool)
    queryStmt (Stmt a (ReturnS _)) = (m, True)
      where
        m = do
          illegalReturn <- ask
          when illegalReturn $
            tell $ Msg.singleton $ mkErrorMsg a IllegalReturn
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
  show = show . pretty

instance Pretty CheckReturnException where
  pretty = go
    where
      go IllegalReturn =
        text "illegal" </>
        text "return" </>
        text "statement"
      go (StrMsgError s) =
        text s
      go NoMsgError =
        text "internal" </>
        text "error"

instance Exception CheckReturnException

instance Error CheckReturnException where
  strMsg = StrMsgError
  noMsg = NoMsgError
