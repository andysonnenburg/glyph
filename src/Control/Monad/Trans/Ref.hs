{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , Rank2Types
  , UndecidableInstances #-}
module Control.Monad.Trans.Ref
       ( RefSupply
       , runRefSupply
       , RefSupplyT
       , runRefSupplyT
       , Ref
       , newRef
       , readRef
       , writeRef
       , modifyRef
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class hiding (get, modify, put)
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.Class
import Control.Monad.Weak
import Control.Monad.Writer.Class

import Data.Functor.Identity
import Data.WeakIntMap.Strict (WeakIntMap, Key)
import qualified Data.WeakIntMap.Strict as IntMap

import GHC.Exts (Any)

import qualified Unsafe.Coerce as Unsafe (unsafeCoerce)

type IntMap = WeakIntMap

type RefSupply s = RefSupplyT s Identity

runRefSupply :: (forall s . RefSupply s a) -> a
runRefSupply = runIdentity . runRefSupplyT

newtype RefSupplyT s m a =
  RefSupplyT { unRefSupplyT :: S -> m (PairS a)
             }

runRefSupplyT :: Monad m => (forall s . RefSupplyT s m a) -> m a
runRefSupplyT m = do
  PairS a _ <- unRefSupplyT m initS
  return a

get :: Monad m => RefSupplyT s m S
get = RefSupplyT $ \ s -> return $! PairS s s

put :: Monad m => S -> RefSupplyT s m ()
put s = RefSupplyT $ \ _ -> return $! PairS () s

modify :: Monad m => (S -> S) -> RefSupplyT s m ()
modify f = RefSupplyT $ \ s -> return $! PairS () (f s)

instance Functor m => Functor (RefSupplyT s m) where
  fmap f m = RefSupplyT $ \ s -> fmap (fmap f) $ unRefSupplyT m s

instance (Functor m, MonadPlus m) => Alternative (RefSupplyT s m) where
  empty = mzero
  (<|>) = mplus

instance (Functor m, Monad m) => Applicative (RefSupplyT s m) where
  pure a = RefSupplyT $ \ s -> return $! PairS a s
  (<*>) = ap

instance Monad m => Monad (RefSupplyT s m) where
  return a = RefSupplyT $ \ s -> return $! PairS a s
  m >>= k = RefSupplyT $ \ s -> do
    PairS a s' <- unRefSupplyT m s
    unRefSupplyT (k a) s'

instance MonadPlus m => MonadPlus (RefSupplyT s m) where
  mzero = RefSupplyT $ \ _ -> mzero
  m `mplus` n = RefSupplyT $ \ s -> unRefSupplyT m s `mplus` unRefSupplyT n s

instance MonadFix m => MonadFix (RefSupplyT s m) where
  mfix f = RefSupplyT $ \ s -> mfix $ \ ~(PairS a _) -> unRefSupplyT (f a) s

instance MonadTrans (RefSupplyT s) where
  lift m = RefSupplyT $ \ s -> do
    a <- m
    return $! PairS a s

instance MonadIO m => MonadIO (RefSupplyT s m) where
  liftIO = lift . liftIO

instance MonadError e m => MonadError e (RefSupplyT s m) where
  throwError = lift . throwError
  catchError m h =
    RefSupplyT $ \ s -> unRefSupplyT m s `catchError` \ e -> unRefSupplyT (h e) s

instance MonadReader r m => MonadReader r (RefSupplyT s m) where
  ask = lift ask
  local = mapRefSupplyT . local
  reader = lift . reader

mapRefSupplyT :: (m (PairS a) -> n (PairS b)) ->
                 RefSupplyT s m a -> RefSupplyT s n b
mapRefSupplyT f m = RefSupplyT $ f . unRefSupplyT m

instance MonadState s' m => MonadState s' (RefSupplyT s m) where
  get = lift State.get
  put = lift . State.put
  state = lift . state

instance MonadWriter w m => MonadWriter w (RefSupplyT s m) where
  writer = lift . writer
  tell = lift . tell
  listen = undefined
  pass = undefined

data PairS a = PairS a !S

instance Functor PairS where
  fmap f (PairS a s) = PairS (f a) s
    
data S = S !Key !(IntMap Value)

newtype Value = Value { unValue :: Any }

initS :: S
initS = S minBound IntMap.empty

newtype Ref s a = Ref Key deriving Show

newRef :: Monad m => a -> RefSupplyT s m (Ref s a)
newRef v = do
  S n m <- get
  put $! S (n + 1) $ unsafeRunWeakM $ IntMap.insert n (toValue v) m
  return $! Ref n

readRef :: Monad m => Ref s a -> RefSupplyT s m a
readRef (Ref k) = do
  S _ m <- get
  return $ fromValue $ unsafeRunWeakM $ IntMap.find k m

writeRef :: Monad m => Ref s a -> a -> RefSupplyT s m ()
writeRef (Ref k) v =
  modify $ \ (S n m) ->
    S n $ unsafeRunWeakM $ IntMap.insert k (toValue v) m

modifyRef :: Monad m => Ref s a -> (a -> a) -> RefSupplyT s m ()
modifyRef (Ref k) f =
  modify $ \ (S n m) -> S n $ unsafeRunWeakM $ IntMap.adjust f' k m
  where
    f' = toValue . f . fromValue

toValue :: v -> Value
toValue = Value . Unsafe.unsafeCoerce

fromValue :: Value -> v
fromValue = Unsafe.unsafeCoerce . unValue
