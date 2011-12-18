{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
  , FlexibleContexts #-}
module Language.Glyph.Location
       ( module Language.Glyph.Annotation.Location
       , LocatedException
       , locatedMsg
       , tellError
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Data

import Language.Glyph.Annotation.Location
import Language.Glyph.Message

data LocatedException
  = forall e. Exception e => LocatedException Location e
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

tellError :: ( Exception e
            , MonadReader Location m
            , MonadWriter Message m
            ) => e -> m ()
tellError x = do
  l <- ask
  tell $ Error $ locatedMsg l x
