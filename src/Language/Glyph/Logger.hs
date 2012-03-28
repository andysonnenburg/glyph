{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , UndecidableInstances #-}
module Language.Glyph.Logger
       ( LoggerT
       , runLoggerT
       , LoggerException (..)
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.Writer

import Data.Typeable

import Language.Glyph.Msg
import Language.Glyph.Unique

import System.Console.Terminfo.PrettyPrint

import Text.PrettyPrint.Free

import Prelude hiding (foldr1)

newtype LoggerT m a
  = LoggerT { unLoggerT :: WriterT Msgs m a
            } deriving ( Functor
                       , Applicative
                       , Monad
                       , MonadIO
                       , MonadTrans
                       , MonadFix
                       )

instance Monad m => MonadWriter Msgs (LoggerT m) where
  tell = LoggerT . tell
  listen = LoggerT . listen . unLoggerT
  pass = LoggerT . pass . unLoggerT

deriving instance MonadError e m => MonadError e (LoggerT m)

data LoggerException = PreviousErrors deriving Typeable

instance Show LoggerException where
  show = show . pretty

instance Exception LoggerException

instance Pretty LoggerException where
  pretty = go
    where
      go PreviousErrors =
        text "failed" <+>
        text "due" <+>
        text "to" <+>
        text "previous" <+>
        text "errors"

runLoggerT :: MonadIO m => LoggerT m a -> m a
runLoggerT m = do
  (a, msgs) <- runWriterT . unLoggerT $ m
  let doc = vcat . map pretty $ msgs :: TermDoc
  liftIO $ do
    display doc
    unless (null msgs) $ putChar '\n'
  return a

instance UniqueMonad m => UniqueMonad (LoggerT m) where
  freshUnique = lift freshUnique
