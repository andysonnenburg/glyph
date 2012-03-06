{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.AddExtraSet
       ( addExtraSet
       ) where

import Control.Monad.Writer

import Data.Graph hiding (Node, scc, vertices)

import Language.Glyph.Annotation.CallSet hiding (callSet)
import qualified Language.Glyph.Annotation.CallSet as Annotation
import Language.Glyph.Annotation.ExtraSet hiding (extraSet)
import Language.Glyph.Annotation.FreeVars hiding (freeVars)
import qualified Language.Glyph.Annotation.FreeVars as Annotation
import Language.Glyph.Annotation.Sort
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!), intersectionWith')
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet
import qualified Language.Glyph.IdentSet as IdentSet

addExtraSet :: ( HasSort sym
              , HasFreeVars sym
              , HasCallSet sym
              , Monad m
              ) => (a, IdentMap sym) -> m (a, IdentMap (Annotated ExtraSet sym))
addExtraSet (stmts, symtab) =
  return (stmts, intersectionWith' (flip withExtraSet) mempty symtab symtab')
  where
    symtab' = execWriter . tellExtraSets . reverse $ nodes
    nodes = map (flattenNodes . flattenSCC) scc
    scc = stronglyConnCompR callGraph
    callGraph = map mkEdge . filter isFun . IdentMap.toList $ symtab
      where
        isFun (_, sym) = sort sym == Fun
        mkEdge (x, sym) = (freeVars, x, callSet)
          where
            freeVars = Annotation.freeVars sym
            callSet = IdentSet.toList $ Annotation.callSet sym

tellExtraSets :: MonadWriter (IdentMap IdentSet) m => [Node] -> m ()
tellExtraSets [] =
  return ()
tellExtraSets ((freeVars, xs, callSet):nodes) = do
  extraSets <- listen' $ tellExtraSets nodes
  let extraSet = freeVars <> mconcat (map (extraSets !) callSet)
  forM_ xs $ \ x ->
    tell $ IdentMap.singleton x extraSet

type Node = (IdentSet, [Ident], [Ident])

flattenNodes :: [(IdentSet, Ident, [Ident])] -> Node
flattenNodes nodes = (freeVars, xs, callSet')
  where
    Node' freeVars xs callSet = mconcat . map mkNode' $ nodes
    callSet' = IdentSet.toList $ callSet \\ IdentSet.fromList xs

data Node' = Node' IdentSet [Ident] IdentSet

mkNode' :: (IdentSet, Ident, [Ident]) -> Node'
mkNode' (freeVars, x, callSet) = Node' freeVars [x] (IdentSet.fromList callSet)

instance Monoid Node' where
  mempty = Node' mempty mempty mempty
  Node' freeVars xs callSet `mappend` Node' freeVars' xs' callSet' =
    Node' (freeVars <> freeVars') (xs <> xs') (callSet <> callSet')

listen' :: MonadWriter w m => m a -> m w
listen' = liftM snd . listen
