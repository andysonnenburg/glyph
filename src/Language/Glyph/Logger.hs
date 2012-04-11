{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Language.Glyph.Logger
       ( LoggerT
       , runLoggerT
       , runErrorT
       , LoggerException (..)
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error hiding (runErrorT)
import qualified Control.Monad.Error as Error
import Control.Monad.Writer.Strict

import Data.Typeable

import Language.Glyph.Msg
import Language.Glyph.Unique

import Text.PrettyPrint.Free

newtype LoggerT m a
  = LoggerT { unLoggerT :: ErrorT WrappedSomeException (WriterT Msgs m) a
            } deriving ( Functor
                       , Applicative
                       , Monad
                       , MonadIO
                       , MonadFix
                       )

instance MonadTrans LoggerT where
  lift = LoggerT . lift . lift

newtype WrappedSomeException
  = WrapSomeException { unwrapSomeException :: SomeException
                      }

instance Error WrappedSomeException where
  strMsg = WrapSomeException . toException . userError
  noMsg = WrapSomeException . toException $ userError "internal error"

instance Monad m => MonadWriter Msgs (LoggerT m) where
  tell = LoggerT . tell
  listen = LoggerT . listen . unLoggerT
  pass = LoggerT . pass . unLoggerT

runErrorT :: (Exception e, Monad m) => ErrorT e (LoggerT m) a -> LoggerT m a
runErrorT = LoggerT . mapErrorT (joinErrorT . liftM (mapLeft f) . unLoggerT)
  where
    f = WrapSomeException . toException

joinErrorT :: Monad m => ErrorT e m (Either e a) -> m (Either e a)
joinErrorT = liftM join . Error.runErrorT

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = go
  where
    go (Left l) = Left (f l)
    go (Right r) = Right r

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
  (a, msgs) <- runWriterT . Error.runErrorT . unLoggerT $ m
  let doc = vcat . map pretty $ msgs
  liftIO $ do
    putStr . show $ doc
    unless (null msgs) $ putChar '\n'
  either (liftIO . throwIO . unwrapSomeException) return a

instance UniqueMonad m => UniqueMonad (LoggerT m) where
  freshUnique = lift freshUnique
