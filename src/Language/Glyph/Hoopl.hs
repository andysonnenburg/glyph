{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Language.Glyph.Hoopl
       ( foldGraphNodesL
       , foldGraphNodesR
       ) where

import Compiler.Hoopl

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
    lift :: forall thing ex . (thing -> b -> b) -> (MaybeO ex thing -> b -> b)
    
    graph GNil = id
    graph (GUnit b) = block b
    graph (GMany e b x) = lift block e . body b . lift block x
    body :: Body n -> b -> b
    body bdy = \a -> mapFold block a bdy
    lift _ NothingO = id
    lift f (JustO thing) = f thing
    
    block :: Block n e x -> IndexedCO x b b -> IndexedCO e b b
    block = foldBlockNodesB f
