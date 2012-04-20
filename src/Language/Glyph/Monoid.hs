{-# LANGUAGE DeriveDataTypeable #-}
module Language.Glyph.Monoid
       ( module X
       , WrappedSemigroup (..)
       , unwrapSemigroup
       ) where

import Data.Data
import Data.Semigroup as X

data WrappedSemigroup a
  = Mempty
  | WrapSemigroup a deriving (Typeable, Data)

instance Semigroup a => Monoid (WrappedSemigroup a) where
  mempty = Mempty
  Mempty `mappend` x = x
  x `mappend` Mempty = x
  WrapSemigroup a `mappend` WrapSemigroup b = WrapSemigroup $ a <> b

unwrapSemigroup :: a -> WrappedSemigroup a -> a
unwrapSemigroup a = unwrap
  where
    unwrap Mempty = a
    unwrap (WrapSemigroup b) = b
