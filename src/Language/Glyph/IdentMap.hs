{-# LANGUAGE DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , TemplateHaskell #-}
module Language.Glyph.IdentMap
       ( IdentMap
       , (!)
       , delete
       , difference
       , elems
       , empty
       , filterWithKey
       , findWithDefault
       , foldWithKey
       , fromList
       , insert
       , intersection
       , intersectionWith
       , keysSet
       , lookup
       , mapWithKey
       , null
       , singleton
       , toList
       , union
       , unionWith
       ) where

import Control.Arrow

import Data.Data
import Data.Foldable hiding (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Monoid
import Data.Traversable

import Language.Glyph.Ident.Internal
import Language.Glyph.IdentSet.Internal (IdentSet (..))
import Language.Haskell.TH.Syntax (showName)
import Language.Haskell.TH as TH

import Prelude hiding (lookup, null)

newtype IdentMap v
  = IdentMap { unIdentMap :: IntMap v
             } deriving ( Show
                        , Monoid
                        , Functor
                        , Foldable
                        , Traversable
                        , Typeable
                        )

instance Data a => Data (IdentMap a) where
  gfoldl f z im = z fromList `f` toList im
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType name
    where
      name = $(return . TH.LitE . TH.StringL . showName $ ''IdentMap)

(!) :: IdentMap v -> Ident -> v
IdentMap m ! Ident k = m IntMap.! k

delete :: Ident -> IdentMap a -> IdentMap a
delete (Ident k) (IdentMap t) = IdentMap $ IntMap.delete k t

difference :: IdentMap a -> IdentMap b -> IdentMap a
difference (IdentMap t1) (IdentMap t2) = IdentMap $ IntMap.difference t1 t2

elems :: IdentMap v -> [v]
elems (IdentMap m) = IntMap.elems m

empty :: IdentMap v
empty = IdentMap IntMap.empty

filterWithKey :: (Ident -> a -> Bool) -> IdentMap a -> IdentMap a
filterWithKey predicate (IdentMap t) =
  IdentMap $ IntMap.filterWithKey (predicate . Ident) t

findWithDefault :: a -> Ident -> IdentMap a -> a
findWithDefault def (Ident k) (IdentMap m) = IntMap.findWithDefault def k m

foldWithKey :: (Ident -> a -> b -> b) -> b -> IdentMap a -> b
foldWithKey f z (IdentMap t) = IntMap.foldWithKey (f . Ident) z t

fromList :: [(Ident, v)] -> IdentMap v
fromList = IdentMap . IntMap.fromList . map (\ (Ident k, v) -> (k, v))

insert :: Ident -> v -> IdentMap v -> IdentMap v
insert (Ident k) x (IdentMap t) = IdentMap $ IntMap.insert k x t

intersection :: IdentMap a -> IdentMap b -> IdentMap a
intersection (IdentMap t1) (IdentMap t2) = IdentMap $ IntMap.intersection t1 t2

intersectionWith :: (a -> b -> c) -> IdentMap a -> IdentMap b -> IdentMap c
intersectionWith f (IdentMap t1) (IdentMap t2) =
  IdentMap $ IntMap.intersectionWith f t1 t2

keysSet :: IdentMap a -> IdentSet
keysSet = IdentSet . IntMap.keysSet . unIdentMap

lookup :: Ident -> IdentMap v -> Maybe v
lookup (Ident k) (IdentMap t) = IntMap.lookup k t

mapWithKey :: (Ident -> a -> b) -> IdentMap a -> IdentMap b
mapWithKey f (IdentMap t) = IdentMap $ IntMap.mapWithKey (f . Ident) t

null :: IdentMap a -> Bool
null (IdentMap t) = IntMap.null t

singleton :: Ident -> v -> IdentMap v
singleton (Ident k) x = IdentMap $ IntMap.singleton k x

toList :: IdentMap v -> [(Ident, v)]
toList = map (first Ident) . IntMap.toList . unIdentMap

union :: IdentMap v -> IdentMap v -> IdentMap v
union (IdentMap t1) (IdentMap t2) = IdentMap $ IntMap.union t1 t2

unionWith :: (v -> v -> v) -> IdentMap v -> IdentMap v -> IdentMap v
unionWith f (IdentMap t1) (IdentMap t2) = IdentMap $ IntMap.unionWith f t1 t2