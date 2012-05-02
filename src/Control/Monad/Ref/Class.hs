{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Control.Monad.Ref.Class
       ( MonadRef (..)
       ) where

import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Ref hiding (modifyRef, newRef, readRef, writeRef)
import qualified Control.Monad.Trans.Ref as Ref
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import Data.IORef
import Data.Monoid
import Data.STRef

class Monad m => MonadRef r m | m -> r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()
  modifyRef :: r a -> (a -> a) -> m ()
  modifyRef r f = do
    a <- readRef r
    writeRef r (f a)

instance Monad m => MonadRef (Ref s) (RefSupplyT s m) where
  newRef = Ref.newRef
  readRef = Ref.readRef
  writeRef = Ref.writeRef
  modifyRef = Ref.modifyRef

instance MonadRef IORef IO where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef

instance MonadRef (STRef s) (ST s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef
  modifyRef = modifySTRef

instance (Error e, MonadRef r m) => MonadRef r (ErrorT e m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r
  modifyRef r = lift . modifyRef r

instance MonadRef r' m => MonadRef r' (ReaderT r m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r
  modifyRef r = lift . modifyRef r

instance (Monoid w, MonadRef r m) => MonadRef r (Lazy.WriterT w m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r
  modifyRef r = lift . modifyRef r

instance (Monoid w, MonadRef r m) => MonadRef r (Strict.WriterT w m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r
  modifyRef r = lift . modifyRef r
