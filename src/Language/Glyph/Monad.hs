module Language.Glyph.Monad
       ( Monad (..)
       , WrappedMonad (..)
       ) where

import qualified Control.Monad as Monad

import Prelude hiding (Monad (..))

class Monad m where
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b
  (>>) :: m i j a -> m j k b -> m i k b
  return :: a -> m i i a
  fail :: String -> m i j a
  
  {-# INLINE (>>) #-}
  m >> n = m >>= \ _ -> n
  fail s = error s

infixl 1 >>=, >>

newtype WrappedMonad m i j a
  = WrapMonad { unwrapMonad :: m a
              }

instance Monad.Monad m => Monad (WrappedMonad m) where
  (WrapMonad m) >>= k = WrapMonad $ m Monad.>>= unwrapMonad . k
  (WrapMonad m) >> (WrapMonad n) = WrapMonad $ m Monad.>> n
  return = WrapMonad . Monad.return
  fail = WrapMonad . Monad.fail