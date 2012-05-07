{-# LANGUAGE GeneralizedNewtypeDeriving, MagicHash, UnboxedTuples #-}
module Control.Monad.Weak
       ( module X
       , WeakM
       , runWeakM
       , mkWeak
       , deRefWeak
       , touch
       , unsafeRunWeakM
       ) where

import Control.Applicative
import Control.Monad.Fix

import System.Mem.Weak as X (Weak)
import qualified System.Mem.Weak as Weak

import GHC.Exts (touch#)
import GHC.Types (IO (..))

import System.IO.Unsafe (unsafeDupablePerformIO)

newtype WeakM a
  = WeakM { runWeakM :: IO a
          } deriving ( Functor
                     , Applicative
                     , Monad
                     , MonadFix
                     )

mkWeak :: k -> v -> WeakM (Weak v)
mkWeak key val = WeakM $ Weak.mkWeak key val Nothing

deRefWeak :: Weak v -> WeakM (Maybe v)
deRefWeak = WeakM . Weak.deRefWeak

touch :: a -> WeakM ()
touch a = WeakM $ IO $ \ s -> case touch# a s of s' -> (# s', () #)

unsafeRunWeakM :: WeakM a -> a
unsafeRunWeakM = unsafeDupablePerformIO . runWeakM
