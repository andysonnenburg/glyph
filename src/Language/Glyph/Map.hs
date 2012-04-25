{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Map
       ( module X
       , Map
       , (\\)
       ) where

import Data.Hashable
import Data.HashMap.Strict as X
import Data.Semigroup

type Map = HashMap

instance (Eq k, Hashable k) => Semigroup (Map k v) where
  (<>) = union

(\\) :: (Eq k, Hashable k) => Map k v -> Map k v' -> Map k v
{-# INLINE (\\) #-}
(\\) = difference
