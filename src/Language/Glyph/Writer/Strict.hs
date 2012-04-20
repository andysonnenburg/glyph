{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Language.Glyph.Writer.Strict
       ( module X
       , WriterT
       , runWriterT
       ) where

import Compiler.Hoopl
import Control.Applicative
import Control.Monad as X
import Control.Monad.Fix as X
import Control.Monad.Trans as X
import Control.Monad.Writer.Class as X

import Data.Monoid as X

data Pair a b = Pair a !b

newtype WriterT w m a = WriterT { unWriterT :: m (Pair a w) }

runWriterT :: Monad m => WriterT w m a -> m (a, w)
runWriterT m = do
  Pair a w <- unWriterT m
  return (a, w)

mapWriterT :: (m (Pair a w) -> n (Pair b w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (unWriterT m)

instance Functor m => Functor (WriterT w m) where
  fmap f = mapWriterT . fmap $ \ (Pair a w) -> Pair (f a) w

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a = WriterT . pure $! Pair a mempty
  f <*> v = WriterT $ liftA2 k (unWriterT f) (unWriterT v)
    where
      k (Pair a w) (Pair b w') = Pair (a b) (w `mappend` w')

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return a = WriterT . return $! Pair a mempty
  {-# INLINE return #-}
  m >>= k = WriterT $ do
    Pair a w <- unWriterT m
    Pair b w' <- unWriterT (k a)
    return $! Pair b (w `mappend` w')
  {-# INLINE (>>=) #-}
  fail msg = WriterT $ fail msg

instance Monoid w => MonadTrans (WriterT w) where
  lift m = WriterT $ do
    a <- m
    return $! Pair a mempty

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
  liftIO = lift . liftIO

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  writer (a, w) = WriterT $ return $! Pair a w
  tell w = WriterT $ return $! Pair () w
  listen m = WriterT $ do
    Pair a w <- unWriterT m
    return $! Pair (a, w) w
  pass m = WriterT $ do
    Pair (a, f) w <- unWriterT m
    return $! Pair a (f w)

instance (Monoid w, UniqueMonad m) => UniqueMonad (WriterT w m) where
  freshUnique = lift freshUnique
