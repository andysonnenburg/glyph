module Language.Glyph.AddExtraSet
       ( addExtraSet
       ) where

import Data.Graph hiding (scc, vertices)

import Language.Glyph.Annotation.CallSet hiding (callSet)
import qualified Language.Glyph.Annotation.CallSet as Annotation
import Language.Glyph.Annotation.ExtraSet hiding (extraSet)
import Language.Glyph.Annotation.FreeVars hiding (freeVars)
import qualified Language.Glyph.Annotation.FreeVars as Annotation
import Language.Glyph.Annotation.Sort
import Language.Glyph.Monoid
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!), intersectionWith')
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet
import qualified Language.Glyph.IdentSet as IdentSet

addExtraSet :: ( HasSort b
              , HasFreeVars b
              , HasCallSet b
              , Monad m
              ) => (a, IdentMap b) -> m (a, IdentMap (Annotated ExtraSet b))
addExtraSet (stmts, symtab) =
  return (stmts, intersectionWith' (flip withExtraSet) mempty symtab symtab')
  where
    symtab' = foldl f mempty . map (mergeSCC . flattenSCC) $ scc
      where
        f extraSets (freeVars, xs, callSet) =
          mconcat (extraSets:map (flip IdentMap.singleton extraSet) xs)
            where
              extraSet = freeVars <> mconcat (map (extraSets!) callSet)
    scc = stronglyConnCompR callGraph
    callGraph = map f . filter p . IdentMap.toList $ symtab
      where
        p (_, info) = sort info == Fun
        f (x, info) = (freeVars, x, callSet)
          where
            freeVars = Annotation.freeVars info
            callSet = IdentSet.toList $ Annotation.callSet info

mergeSCC :: [(IdentSet, Ident, [Ident])] -> (IdentSet, [Ident], [Ident])
mergeSCC vertices =
  (freeVars, xs, IdentSet.toList $ callSet \\ IdentSet.fromList xs)
  where
    (freeVars, xs, callSet) = foldr mergeVertex (mempty, mempty, mempty) vertices

mergeVertex :: (IdentSet, Ident, [Ident]) ->
              (IdentSet, [Ident], IdentSet) ->
              (IdentSet, [Ident], IdentSet)
mergeVertex (freeVars, x, callSet) (freeVars', xs, callSet') =
  (freeVars <> freeVars', x:xs, IdentSet.fromList callSet <> callSet')
