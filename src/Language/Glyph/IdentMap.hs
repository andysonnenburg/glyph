{-# LANGUAGE CPP #-}
module Language.Glyph.IdentMap
       ( module X
       , IdentMap
       , intersectionWith
       , intersectionWith'
       , (\\)
       ) where

import Data.Hashable
import Data.HashMap.Lazy as X

import Language.Glyph.Ident
import Language.Glyph.Map ()

import Prelude hiding (lookup)

intersectionWith :: ( Eq k
                    , Hashable k
                    ) => (a -> b -> c) -> HashMap k a -> HashMap k b -> HashMap k c
intersectionWith f a b = foldlWithKey' go empty a
  where
    go m k v =
      case lookup k b of
        Just v' -> insert k (f v v') m
        _ -> m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE intersectionWith #-}
#endif

intersectionWith' :: (Eq k, Hashable k) =>
                     (a -> b -> c) -> b -> HashMap k a -> HashMap k b -> HashMap k c
intersectionWith' f a m1 m2 =
  intersectionWith f m1 m2 `union` fmap (`f` a) m1

(\\) :: (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
(\\) = difference

type IdentMap = HashMap Ident
