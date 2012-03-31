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
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

instance UniqueMonad m => UniqueMonad (ReaderT r m) where
  freshUnique = lift freshUnique

instance (Monoid w, UniqueMonad m) => UniqueMonad (WriterT w m) where
  freshUnique = lift freshUnique

type UniqueSupply a = UniqueSupplyT Identity a

runUniqueSupply :: UniqueSupply a -> a
runUniqueSupply = runIdentity . runUniqueSupplyT

newtype UniqueSupplyT m a
  = UniqueSupplyT { unUniqueSupplyT :: StateT Int m a
                  } deriving ( Functor
                             , Applicative
                             , Monad
                             , MonadIO
                             , MonadTrans
                             )

deriving instance MonadWriter w m => MonadWriter w (UniqueSupplyT m)

runUniqueSupplyT :: Monad m => UniqueSupplyT m a -> m a
runUniqueSupplyT = flip evalStateT 0 . unUniqueSupplyT

instance Monad m => UniqueMonad (UniqueSupplyT m) where
  freshUnique = UniqueSupplyT $ do
    i <- get
    put $ i + 1
    return $ intToUnique i
