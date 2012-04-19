module Language.Glyph.Logger
       ( LoggerT
       , runLoggerT
       , runErrorT
       ) where

import Control.Exception
import Control.Monad.Error hiding (runErrorT)
import qualified Control.Monad.Error as Error

import Data.Foldable

import Language.Glyph.Msg
import Language.Glyph.Writer.Strict

import Text.PrettyPrint.Free

import System.IO

type LoggerT m = ErrorT WrappedSomeException (WriterT Msgs m)

newtype WrappedSomeException
  = WrapSomeException { unwrapSomeException :: SomeException
                      }

instance Error WrappedSomeException where
  strMsg = WrapSomeException . toException . userError
  noMsg = WrapSomeException . toException $ userError "internal error"

runErrorT :: (Exception e, Monad m) => ErrorT e (LoggerT m) a -> LoggerT m a
runErrorT = mapErrorT (joinErrorT . liftM (mapLeft f))
  where
    f = WrapSomeException . toException

joinErrorT :: Monad m => ErrorT e m (Either e a) -> m (Either e a)
joinErrorT = liftM join . Error.runErrorT

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = go
  where
    go (Left l) = Left (f l)
    go (Right r) = Right r

runLoggerT :: MonadIO m => LoggerT m a -> m a
runLoggerT m = do
  (a, msgs) <- runWriterT . Error.runErrorT $ m
  let msgs' = toList msgs
      doc = vcat . map pretty $ msgs'
  liftIO $ do
    hPutStr stderr . show $ doc
    unless (null msgs') $ hPutChar stderr '\n'
  either (liftIO . throwIO . unwrapSomeException) return a
