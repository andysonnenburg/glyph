{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Language.Glyph.Hoopl
       ( foldGraphNodesL
       , foldGraphNodesR
       , gSplice
       , WrappedSemigroupoid (..)
       , unwrapSemigroupoid
       , WrappedGraph (..)
       ) where

import Compiler.Hoopl
import Control.Category

import Data.Semigroupoid (Semigroupoid (..))

import Prelude hiding ((.), id)

foldGraphNodesL :: forall n a .
                   (forall e x . a -> n e x -> a) ->
                   (forall e x . a -> Graph n e x -> a)
foldGraphNodesL f = flip $ foldGraphNodes (flip f)

foldGraphNodesR :: forall n b .
                   (forall e x . n e x -> b -> b) ->
                   (forall e x . b -> Graph n e x -> b)
foldGraphNodesR f = flip graph
  where
    graph :: forall e x . Graph n e x -> b -> b
    lift :: forall thing ex . (thing -> b -> b) -> MaybeO ex thing -> b -> b

    graph GNil = id
    graph (GUnit b) = block b
    graph (GMany e b x) = lift block e . body b . lift block x
    body :: Body n -> b -> b
    body bdy a = mapFold block a bdy
    lift _ NothingO = id
    lift f (JustO thing) = f thing

    block :: Block n e x -> IndexedCO x b b -> IndexedCO e b b
    block = foldBlockNodesB f

gSplice :: forall n e a x . NonLocal n => (Graph n e a -> Graph n a x -> Graph n e x)
gSplice = sp
  where sp :: forall e a x . Graph n e a -> Graph n a x -> Graph n e x

        sp GNil g2 = g2
        sp g1 GNil = g1

        sp g1@(GUnit _) g2@(GUnit _) = g1 <*> g2

        sp g1@(GUnit _) g2@(GMany (JustO _) _ _) = g1 <*> g2

        sp g1@(GMany _ _ (JustO _)) g2@(GUnit _) = g1 <*> g2

        sp g1@(GMany _ _ (JustO _)) g2@(GMany (JustO _) _ _) = g1 <*> g2

        sp g1@(GMany _ _ NothingO) g2@(GMany NothingO _ _) = g1 |*><*| g2

        sp _ _ = error "bogus GADT match failure"


data WrappedSemigroupoid k a b where
  Id :: WrappedSemigroupoid k a a
  Semi :: k a b -> WrappedSemigroupoid k a b

instance Semigroupoid k => Category (WrappedSemigroupoid k) where
  id = Id
  Id . f = f
  f . Id = f
  Semi g . Semi f = Semi $ g `o` f

unwrapSemigroupoid :: k ex ex -> WrappedSemigroupoid k ex ex -> k ex ex
unwrapSemigroupoid f = unwrap
  where
    unwrap Id = f
    unwrap (Semi g) = g

newtype WrappedGraph n e x = WrapGraph { unwrapGraph :: Graph n e x }

instance NonLocal n => Semigroupoid (WrappedGraph n) where
  WrapGraph g `o` WrapGraph f = WrapGraph $ gSplice f g
