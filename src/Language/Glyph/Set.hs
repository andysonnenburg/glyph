{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Set
       ( module X
       , Set
       , (\\)
       ) where

import Data.Hashable
import Data.HashSet as X
import Data.Semigroup

type Set = HashSet

instance (Eq a, Hashable a) => Semigroup (Set a) where
  (<>) = union

(\\) :: (Eq a, Hashable a) => Set a -> Set a -> Set a
{-# INLINE (\\) #-}
(\\) = difference
