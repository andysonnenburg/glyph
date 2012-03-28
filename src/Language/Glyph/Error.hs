{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , PatternGuards
  , UndecidableInstances #-}
module Language.Glyph.Error
       ( ErrorT
       , runErrorT
       ) where

import Compiler.Hoopl

import Control.Applicative
import Control.Exception
import Control.Monad.Error hiding (ErrorT (..))

import Prelude hiding (catch)

newtype ErrorT s m a
  = ErrorT { unErrorT :: m (Either SomeException a)
           }

runErrorT :: MonadIO m => ErrorT s m a -> m a
runErrorT m = do
  a <- unErrorT m
  case a of
    Left l -> liftIO $ throwIO l
    Right r -> return r
    

instance Functor m => Functor (ErrorT s m) where
  fmap f = ErrorT . fmap (fmap f) . unErrorT

instance (Functor m, Monad m) => Applicative (ErrorT s m) where
  pure = ErrorT . return . Right
  f <*> v = ErrorT $ do
    mf <- unErrorT f
    case mf of
      Left e ->
        return (Left e)
      Right k -> do
        mv <- unErrorT v
        case mv of 
          Left e ->
            return (Left e)
          Right a ->
            return . Right . k $ a

instance Monad m => Monad (ErrorT s m) where
  return = ErrorT . return . Right
  m >>= k = ErrorT $ do
    a <- unErrorT m
    case a of
      Left l -> return (Left l)
      Right r -> unErrorT (k r)
  fail = ErrorT . return . Left . toException . userError

instance MonadTrans (ErrorT s) where
  lift m = ErrorT $ do
    a <- m
    return (Right a)

instance MonadIO m => MonadIO (ErrorT s m) where
  liftIO = lift . liftIO

instance (Exception e, Monad m) => MonadError e (ErrorT s m) where
  throwError = ErrorT . return . Left . toException
  m `catchError` h = ErrorT $ do
    a <- unErrorT m
    case a of
      Left l | Just e <- fromException l -> unErrorT (h e)
      Left l -> return (Left l)
      Right r -> return (Right r)

instance UniqueMonad m => UniqueMonad (ErrorT s m) where
  freshUnique = lift freshUnique
