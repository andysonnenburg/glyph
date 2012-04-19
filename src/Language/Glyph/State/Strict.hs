{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Language.Glyph.State.Strict
       ( module X
       , StateT
       , runStateT
       , evalStateT
       , execStateT
       ) where

import Compiler.Hoopl
import Control.Applicative
import Control.Monad as X
import Control.Monad.Error.Class
import Control.Monad.Fix as X
import Control.Monad.Trans as X
import Control.Monad.State.Class as X
import Control.Monad.Writer.Class

data Pair a b = Pair a !b

newtype StateT s m a = StateT { unStateT :: s -> m (Pair a s) }

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT m s = do
  Pair a s' <- unStateT m s
  return (a, s')

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = do
  Pair a _ <- unStateT m s
  return a

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = do
  Pair _ s' <- unStateT m s
  return s'

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ \ s ->
    fmap (\ (Pair a s') -> Pair (f a) s') $ unStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \ s -> return $! Pair a s
  {-# INLINE return #-}
  m >>= k = StateT $ \ s -> do
    Pair a s' <- unStateT m s
    unStateT (k a) s'
  {-# INLINE (>>=) #-}
  fail str = StateT $ \ _ -> fail str

instance MonadTrans (StateT s) where
  lift m = StateT $ \ s -> do
    a <- m
    return $! Pair a s

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance Monad m => MonadState s (StateT s m) where
  get = StateT $ \ s -> return $! Pair s s
  put s = StateT $ \ _ -> return $! Pair () s
  state f = StateT $ \ s -> do
    let (a, s') = f s
    return $! Pair a s'

instance MonadError e m => MonadError e (StateT s m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

liftCatch :: (m (Pair a s) -> (e -> m (Pair a s)) -> m (Pair a s)) ->
             StateT s m a -> (e -> StateT s m a) -> StateT s m a
liftCatch catchError m h =
  StateT $ \ s -> unStateT m s `catchError` \ e -> unStateT (h e) s

instance MonadWriter w m => MonadWriter w (StateT s m) where
  writer = lift . writer
  tell = lift . tell
  listen = liftListen listen
  pass = liftPass pass

liftListen :: Monad m =>
              (m (Pair a s) -> m (Pair a s, w)) ->
               StateT s m a -> StateT s m (a, w)
liftListen listen m = StateT $ \ s -> do
  (Pair a s', w) <- listen (unStateT m s)
  return $! Pair (a, w) s'

liftPass :: Monad m =>
            (m (Pair a s, b) -> m (Pair a s)) ->
            StateT s m (a, b) -> StateT s m a
liftPass pass m = StateT $ \ s -> pass $ do
  Pair (a, f) s' <- unStateT m s
  return (Pair a s', f)

instance UniqueMonad m => UniqueMonad (StateT s m) where
  freshUnique = lift freshUnique
