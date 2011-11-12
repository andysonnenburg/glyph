{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Language.Glyph.IdentMap
       ( IdentMap
       , (!)
       , elems
       , empty
       , filterWithKey
       , foldWithKey
       , fromList
       , insert
       , intersectionWith
       , lookup
       , mapWithKey
       , singleton
       , toList
       , union
       , unionWith
       ) where

import Data.Foldable hiding (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Monoid
import Data.Traversable
import Data.Typeable

import Language.Glyph.Internal.Ident

import Prelude hiding (lookup)

newtype IdentMap v
  = IdentMap { unIdentMap :: IntMap v
             } deriving ( Show
                        , Monoid
                        , Functor
                        , Foldable
                        , Traversable
                        , Typeable
                        )

(!) :: IdentMap v -> Ident -> v
IdentMap m ! Ident k = m IntMap.! k

elems :: IdentMap v -> [v]
elems (IdentMap m) = IntMap.elems m

empty :: IdentMap v
empty = IdentMap IntMap.empty

filterWithKey :: (Ident -> a -> Bool) -> IdentMap a -> IdentMap a
filterWithKey predicate (IdentMap t) =
  IdentMap $ IntMap.filterWithKey (predicate . Ident) t

foldWithKey :: (Ident -> a -> b -> b) -> b -> IdentMap a -> b
foldWithKey f z (IdentMap t) = IntMap.foldWithKey (f . Ident) z t

fromList :: [(Ident, v)] -> IdentMap v
fromList = IdentMap . IntMap.fromList . map (\ (Ident k, v) -> (k, v))

insert :: Ident -> v -> IdentMap v -> IdentMap v
insert (Ident k) x (IdentMap t) = IdentMap $ IntMap.insert k x t

intersectionWith :: (a -> b -> c) -> IdentMap a -> IdentMap b -> IdentMap c
intersectionWith f (IdentMap t1) (IdentMap t2) =
  IdentMap $ IntMap.intersectionWith f t1 t2

lookup :: Ident -> IdentMap v -> Maybe v
lookup (Ident k) (IdentMap t) = IntMap.lookup k t

mapWithKey :: (Ident -> a -> b) -> IdentMap a -> IdentMap b
mapWithKey f (IdentMap t) = IdentMap $ IntMap.mapWithKey (f . Ident) t

singleton :: Ident -> v -> IdentMap v
singleton (Ident k) x = IdentMap $ IntMap.singleton k x

toList :: IdentMap v -> [(Ident, v)]
toList = map (\ (k, v) -> (Ident k, v)) . IntMap.toList . unIdentMap

union :: IdentMap v -> IdentMap v -> IdentMap v
union (IdentMap t1) (IdentMap t2) = IdentMap $ IntMap.union t1 t2

unionWith :: (v -> v -> v) -> IdentMap v -> IdentMap v -> IdentMap v
unionWith f (IdentMap t1) (IdentMap t2) = IdentMap $ IntMap.unionWith f t1 t2