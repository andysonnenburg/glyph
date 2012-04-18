{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Unique
       ( Unique
       , UniqueMonad (..)
       , UniqueSupply
       , runUniqueSupply
       , UniqueSupplyT
       , runUniqueSupplyT
       ) where

import Compiler.Hoopl
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer

import Language.Glyph.Stream

import Prelude hiding (enumFrom)

instance (Error e, UniqueMonad m) => UniqueMonad (ErrorT e m) where
  freshUnique = lift freshUnique

instance UniqueMonad m => UniqueMonad (ReaderT r m) where
  freshUnique = lift freshUnique

instance UniqueMonad m => UniqueMonad (StateT s m) where
  freshUnique = lift freshUnique

instance (Monoid w, UniqueMonad m) => UniqueMonad (WriterT w m) where
  freshUnique = lift freshUnique

type UniqueSupply a = UniqueSupplyT Identity a

runUniqueSupply :: UniqueSupply a -> a
runUniqueSupply = runIdentity . runUniqueSupplyT

newtype UniqueSupplyT m a
  = UniqueSupplyT { unUniqueSupplyT :: StateT (Stream Int) m a
                  } deriving ( Functor
                             , Applicative
                             , Monad
                             , MonadIO
                             , MonadTrans
                             )

deriving instance MonadWriter w m => MonadWriter w (UniqueSupplyT m)

runUniqueSupplyT :: Monad m => UniqueSupplyT m a -> m a
runUniqueSupplyT = flip evalStateT (enumFrom 0) . unUniqueSupplyT

instance Monad m => UniqueMonad (UniqueSupplyT m) where
  freshUnique = UniqueSupplyT $ do
    x :| xs <- get
    put xs
    return $! intToUnique x
