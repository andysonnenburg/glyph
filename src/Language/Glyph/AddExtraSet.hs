{-# LANGUAGE PatternGuards #-}
module Language.Glyph.AddExtraSet
       ( addExtraSet
       ) where

import Control.Applicative

import Data.Foldable (foldl')
import Data.Graph hiding (scc, vertices)

import Language.Glyph.Annotation.CallSet
import Language.Glyph.Annotation.ExtraSet hiding (extraSet)
import Language.Glyph.Annotation.FreeVars
import Language.Glyph.Annotation.Sort
import Language.Glyph.Monoid
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet
import qualified Language.Glyph.IdentSet as IdentSet

addExtraSet :: ( HasSort a
              , HasFreeVars a
              , HasCallSet a
              , Monad m
              ) => IdentMap a -> m (IdentMap (With a ExtraSet))
addExtraSet symtab =
  return $
  IdentMap.intersectionWith withExtraSet symtab extraSets <>
  ((`withExtraSet` mempty) <$> symtab)
  where
    extraSets = foldl' f mempty . map (concatVertices . flattenSCC) $ scc
      where
        f a (fvs, idents, cs) =
          mconcat (a:map (\ ident -> IdentMap.singleton ident extraSet') idents)
          where
            extraSet' = fvs <> extraSet
            extraSet = mconcat . map (a!) $ cs
    scc = stronglyConnCompR callGraph
    callGraph = IdentMap.foldWithKey f [] symtab
      where
        f k v xs
          | Fun <- sort v = (freeVars v, k, IdentSet.toList (callSet v)):xs
          | otherwise = xs

concatVertices :: [(IdentSet, Ident, [Ident])] -> (IdentSet, [Ident], [Ident])
concatVertices vertices =
  let (fvs, xs, cs) = foldr f (mempty, [], mempty) vertices
  in (fvs, xs, IdentSet.toList $ cs \\ IdentSet.fromList xs)
  where
    f (fvs, x, cs) (fvs', xs, cs') =
      (fvs <> fvs', x:xs, IdentSet.fromList cs <> cs')
