{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.AddNestedFuns
       ( addNestedFuns
       ) where

import Control.Comonad
import Control.Monad.Reader

import Data.Generics
import Data.Maybe

import Language.Glyph.Annotation.NestedFuns
import Language.Glyph.Annotation.Sort
import Language.Glyph.Location
import Language.Glyph.NameMap (Name, NameMap)
import qualified Language.Glyph.NameMap as NameMap
import Language.Glyph.NameSet (NameSet)
import qualified Language.Glyph.NameSet as NameSet
import Language.Glyph.Syntax

addNestedFuns :: ( HasSort a
                , MonadReader [Located (Stmt Name)] m
                ) => NameMap a -> m (NameMap (With a NestedFuns))
addNestedFuns symtab = do
  ss <- ask
  let nfs = nestedFunsQ ss
      lookupNestedFuns k = fromMaybe NameSet.empty (NameMap.lookup k nfs)
      f k v = withNestedFuns v (lookupNestedFuns k)
  return $ NameMap.mapWithKey f symtab

nestedFunsQ :: Data a => a -> NameMap NameSet
nestedFunsQ = everything NameMap.union (NameMap.empty `mkQ` qS `extQ` qE)
  where
    qS (FunDeclS x _ ss) = NameMap.singleton (name x) (funsQ ss)
    qS _ = NameMap.empty
    
    qE (FunE x _ ss) = NameMap.singleton x (funsQ ss)
    qE _ = NameMap.empty

funsQ :: Data a => a -> NameSet
funsQ = everythingBut NameSet.union ((NameSet.empty, False) `mkQ` qS `extQ` qE)
  where
    qS (FunDeclS x _ _) = (NameSet.singleton (name x), True)
    qS _ = (NameSet.empty, False)
    
    qE (FunE x _ _) = (NameSet.singleton x, True)
    qE _ = (NameSet.empty, False)

name :: (Comonad w, Comonad v) => w (v a) -> a
name = extract . extract
