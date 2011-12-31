{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
  , FlexibleContexts #-}
module Language.Glyph.Location
       ( module Language.Glyph.Annotation.Location
       , LocatedException
       , locatedMsg
       , logError
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader

import Data.Data

import Language.Glyph.Annotation.Location
import Language.Glyph.Logger
import Language.Glyph.Message

import Prelude hiding (log)

data LocatedException
  = forall e . Exception e => LocatedException Location e
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show LocatedException where
  show x =
    case x of
      LocatedException l e -> show l ++ ": " ++ show e
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception LocatedException

instance Error LocatedException where
  strMsg = StrMsgError
  noMsg = NoMsgError

locatedMsg :: Exception e => Location -> e -> LocatedException
locatedMsg = LocatedException

logError :: ( Exception e
           , MonadReader Location m
           , MonadLogger Message m
           ) => e -> m ()
logError x = do
  l <- ask
  log $ Error $ locatedMsg l x
