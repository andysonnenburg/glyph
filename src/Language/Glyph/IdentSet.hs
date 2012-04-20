{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.IdentSet
       ( module X
       , IdentSet
       , (\\)
       ) where

import Data.Hashable
import Data.HashSet as X
import Data.Semigroup

import Prelude (Eq)

import Language.Glyph.Ident

instance (Eq a, Hashable a) => Semigroup (HashSet a) where
  (<>) = union

(\\) :: (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
{-# INLINE (\\) #-}
(\\) = difference

type IdentSet = HashSet Ident
