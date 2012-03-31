{-# LANGUAGE
    FlexibleContexts
  , PatternGuards
  , TypeOperators #-}
module Language.Glyph.AddExtraSet
       ( addExtraSet
       ) where

import Control.Monad.Writer

import Data.Graph hiding (Node, scc, vertices)

import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Record hiding (Sort, Symtab)
import qualified Language.Glyph.Record as Record
import Language.Glyph.Sort

type Symtab sym = IdentMap (Record sym)
type Symtab' sym = IdentMap (Record ('(ExtraSet, IdentSet) ': sym))

addExtraSet :: ( Select Record.Sort Sort sym
               , Select FreeVars IdentSet sym
               , Select CallSet IdentSet sym
               , Select Record.Symtab (Symtab sym) fields
               , Remove Record.Symtab fields fields'
               , Monad m
               ) =>
               Record fields ->
               m (Record ('(Record.Symtab, Symtab' sym) ': fields'))
addExtraSet r = return $ symtab #= symtab'' #| (r #- symtab)
  where
    symtab' = r#.symtab
    symtab'' =
      IdentMap.intersectionWith'
      (\ r' x -> extraSet #= x #| r') mempty symtab' extraSets
    extraSets = execWriter . tellExtraSets . reverse $ nodes
    nodes = map (flattenNodes . flattenSCC) scc
    scc = stronglyConnCompR callGraph
    callGraph = map mkEdge . filter isFun . IdentMap.toList $ symtab'
      where
        isFun (_, sym)
          | Fun <- sym#.sort = True
          | otherwise = False
        mkEdge (x, sym) = (freeVars', x, callSet')
          where
            freeVars' = sym#.freeVars
            callSet' = IdentSet.toList $ sym#.callSet

tellExtraSets :: MonadWriter (IdentMap IdentSet) m => [Node] -> m ()
tellExtraSets [] =
  return ()
tellExtraSets ((freeVars', xs, callSet'):nodes) = do
  extraSets <- listen' $ tellExtraSets nodes
  let extraSet' = freeVars' <> mconcat (map (extraSets !) callSet')
  forM_ xs $ \ x ->
    tell $ IdentMap.singleton x extraSet'

type Node = (IdentSet, [Ident], [Ident])

flattenNodes :: [(IdentSet, Ident, [Ident])] -> Node
flattenNodes nodes = (freeVars', xs, callSet'')
  where
    Node' freeVars' xs callSet' = mconcat . map mkNode' $ nodes
    callSet'' = IdentSet.toList $ callSet' \\ IdentSet.fromList xs

data Node' = Node' IdentSet [Ident] IdentSet

mkNode' :: (IdentSet, Ident, [Ident]) -> Node'
mkNode' (freeVars', x, callSet') = Node' freeVars' [x] (IdentSet.fromList callSet')

instance Monoid Node' where
  mempty = Node' mempty mempty mempty
  Node' freeVars' xs callSet' `mappend` Node' freeVars'' xs' callSet'' =
    Node' (freeVars' <> freeVars'') (xs <> xs') (callSet' <> callSet'')

listen' :: MonadWriter w m => m a -> m w
listen' = liftM snd . listen
