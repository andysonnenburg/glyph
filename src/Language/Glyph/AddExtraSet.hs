{-# LANGUAGE PatternGuards #-}
module Language.Glyph.AddExtraSet
       ( addExtraSet
       ) where

import Control.Applicative

import Data.Graph hiding (scc, vertices)

import Language.Glyph.Annotation.CallSet hiding (callSet)
import qualified Language.Glyph.Annotation.CallSet as Annotation
import Language.Glyph.Annotation.ExtraSet hiding (extraSet)
import Language.Glyph.Annotation.FreeVars hiding (freeVars)
import qualified Language.Glyph.Annotation.FreeVars as Annotation
import Language.Glyph.Annotation.Sort
import Language.Glyph.Monoid
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet
import qualified Language.Glyph.IdentSet as IdentSet

addExtraSet :: ( HasSort b
              , HasFreeVars b
              , HasCallSet b
              , Monad m
              ) => (a, IdentMap b) -> m (a, IdentMap (Annotated ExtraSet b))
addExtraSet (stmts, symtab) =
  return (stmts,
          IdentMap.intersectionWith (flip withExtraSet) symtab extraSets <>
          (withExtraSet mempty <$> symtab))
  where
    extraSets = foldl f mempty . map (mergeSCC . flattenSCC) $ scc
      where
        f extraSets (freeVars, xs, callSet) =
          mconcat (extraSets:map (flip IdentMap.singleton extraSet) xs)
            where
              extraSet = freeVars <> (mconcat $ map (extraSets!) callSet)
    scc = stronglyConnCompR callGraph
    callGraph = IdentMap.foldWithKey f [] symtab
      where
        f sym info callSets
          | Fun <- sort info =
            (Annotation.freeVars info,
             sym,
             IdentSet.toList (Annotation.callSet info)):callSets
          | otherwise =
            callSets

mergeSCC :: [(IdentSet, Ident, [Ident])] -> (IdentSet, [Ident], [Ident])
mergeSCC vertices =
  let (freeVars, xs, callSet) = foldr f (mempty, [], mempty) vertices
  in (freeVars, xs, IdentSet.toList $ callSet \\ IdentSet.fromList xs)
  where
    f (freeVars, x, callSet) (freeVars', xs, callSet') =
      (freeVars <> freeVars', x:xs, IdentSet.fromList callSet <> callSet')
