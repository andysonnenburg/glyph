{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Language.Glyph.Hoopl.Monad.Writer.Internal
       ( Writer
       , runWriter
       , execWriter
       , WriterT
       , runWriterT
       , execWriterT
       , mapWriterT
       , lift
       , tell
       , ask
       , asks
       , local
       , throwError
       ) where

import Compiler.Hoopl
import Control.Category
import Control.Monad (liftM)
import Control.Monad.Error.Class (MonadError)
import qualified Control.Monad.Error.Class as Error
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader.Class (MonadReader)
import qualified Control.Monad.Reader.Class as Reader

import qualified Language.Glyph.Hoopl.Monad as Hoopl

import Prelude hiding ((.), id)

type Writer w = WriterT w Identity

runWriter :: Writer w e x a -> (a, w e x)
runWriter = runIdentity . runWriterT

execWriter :: Writer w e x a -> w e x
execWriter m = snd (runWriter m)

newtype WriterT w m e x a = WriterT { runWriterT :: m (a, w e x) }

execWriterT :: Monad m => WriterT w m e x a -> m (w e x)
execWriterT m = do
  ~(_, w) <- runWriterT m
  return w

mapWriterT :: (m (a, w e x) -> n (b, w' e x)) -> WriterT w m e x a -> WriterT w' n e x b
mapWriterT f m = WriterT $ f (runWriterT m)

instance (Category w, Monad m) => Hoopl.Monad (WriterT w m) where
  return a = WriterT $ return (a, id)
  m >>= k = WriterT $ do
    ~(a, w) <- runWriterT m
    ~(b, w') <- runWriterT (k a)
    return (b, w >>> w')
  fail msg = WriterT $ fail msg

instance (Category w, Monad m) => Monad (WriterT w m ex ex) where
  (>>=) = (Hoopl.>>=)
  (>>) = (Hoopl.>>)
  return = Hoopl.return
  fail = Hoopl.fail
  
lift :: (Category w, Monad m) => m a -> WriterT w m ex ex a
lift m = WriterT $ do
  a <- m
  return (a, id)

liftCatch :: (m (a, w e x) -> (e' -> m (a, w e x)) -> m (a, w e x)) ->
             WriterT w m e x a -> (e' -> WriterT w m e x a) -> WriterT w m e x a
liftCatch catchError' m h =
  WriterT $ runWriterT m `catchError'` \ e -> runWriterT (h e)

tell :: (Category w, Monad m) => w e x -> WriterT w m e x ()
tell w = WriterT $ return ((), w)

ask :: (Category w, MonadReader r m) => WriterT w m ex ex r
ask = lift Reader.ask

asks :: (Category w, MonadReader r m) => (r -> a) -> WriterT w m ex ex a
asks f = liftM f ask

local :: MonadReader r m => (r -> r) -> WriterT w m e x a -> WriterT w m e x a
local = mapWriterT . Reader.local

throwError :: MonadError e' m => e' -> WriterT w m e x a
throwError = WriterT . Error.throwError

instance (Category w, MonadError e m) => MonadError e (WriterT w m ex ex) where
  throwError = throwError
  catchError = liftCatch Error.catchError

instance (Category w, UniqueMonad m) => UniqueMonad (WriterT w m ex ex) where
  freshUnique = lift freshUnique
