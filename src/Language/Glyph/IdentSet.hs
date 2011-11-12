{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Language.Glyph.IdentSet
       ( IdentSet
       , (\\)
       , empty
       , fromList
       , insert
       , map
       , member
       , notMember
       , null
       , singleton
       , toList
       , union
       , unions
       ) where

import Data.Data
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Monoid

import Language.Glyph.Internal.Ident
import Language.Haskell.TH.Syntax (showName)
import Language.Haskell.TH as TH

import Prelude hiding (null)

newtype IdentSet
  = IdentSet { unIdentSet :: IntSet
             } deriving (Show, Monoid, Typeable)

instance Data IdentSet where
  gfoldl f z ns = z fromList `f` (toList ns)
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType name
    where
      name = $(return . TH.LitE . TH.StringL . showName $ ''IdentSet)

infixl 9 \\{-This comment teaches CPP correct behaviour -}
(\\) :: IdentSet -> IdentSet -> IdentSet
IdentSet m1 \\ IdentSet m2 = IdentSet $ IntSet.difference m1 m2

empty :: IdentSet
empty = IdentSet IntSet.empty

fromList :: [Ident] -> IdentSet
fromList = IdentSet . IntSet.fromList . map (\ (Ident x) -> x)

insert :: Ident -> IdentSet -> IdentSet
insert (Ident x) (IdentSet t) = IdentSet $ IntSet.insert x t

member :: Ident -> IdentSet -> Bool
member (Ident x) (IdentSet t) = IntSet.member x t

notMember :: Ident -> IdentSet -> Bool
notMember (Ident x) (IdentSet t) = IntSet.notMember x t

null :: IdentSet -> Bool
null = IntSet.null . unIdentSet

singleton :: Ident -> IdentSet
singleton (Ident x) = IdentSet $ IntSet.singleton x

toList :: IdentSet -> [Ident]
toList = map Ident . IntSet.toList . unIdentSet

union :: IdentSet -> IdentSet -> IdentSet
union (IdentSet t1) (IdentSet t2) = IdentSet $ IntSet.union t1 t2

unions :: [IdentSet] -> IdentSet
unions = IdentSet . IntSet.unions . map unIdentSet