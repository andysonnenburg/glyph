{-# LANGUAGE FlexibleContexts, PatternGuards, ViewPatterns #-}
module Language.Glyph.AddCallSet
       ( addCallSet
       ) where

import Control.Applicative

import Language.Glyph.Annotation.CallSet hiding (callSet)
import Language.Glyph.Annotation.Sort
import Language.Glyph.Generics
import Language.Glyph.Monoid
import Language.Glyph.IdentMap (IdentMap, (!))
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Syntax

addCallSet :: ( Data a
             , HasSort b
             , Monad m
             ) => ([Stmt a], IdentMap b) -> m ([Stmt a], IdentMap (Annotated CallSet b))
addCallSet (stmts, symtab) = do
  let symtab' = callSetsQ symtab stmts
  return (stmts,
          IdentMap.intersectionWith (flip withCallSet) symtab symtab' <>
          (withCallSet mempty <$> symtab))

callSetsQ :: (HasSort a, Data b) => IdentMap a -> b -> IdentMap IdentSet
callSetsQ symtab =
  everythingButFuns (<>)
  (const mempty `ext1Q` queryStmt `ext1Q` queryExpr)
  where
    queryStmt (FunDeclS name _ stmts) =
      queryFun (ident name) stmts
    queryStmt _ =
      mempty
    
    queryExpr (FunE x _ stmts) =
      queryFun x stmts
    queryExpr _ =
      mempty
    
    queryFun x stmts =
      callSets <>
      IdentMap.singleton x (nestedCallSet <> funVars \\ nestedFuns)
      where
        nestedFuns = nestedFunsQ stmts
        funVars = funVarsQ symtab stmts
        nestedCallSet = mconcat (IdentMap.elems callSets)
        callSets = callSetsQ symtab stmts

nestedFunsQ :: Data a => a -> IdentSet
nestedFunsQ =
  everything (<>)
  (const mempty `ext1Q` queryStmt `ext1Q` queryExpr)
  where
    queryStmt (FunDeclS (ident -> x) _ _) =
      IdentSet.singleton x
    queryStmt _ = 
      mempty
    
    queryExpr (FunE x _ _) =
      IdentSet.singleton x
    queryExpr _ =
      mempty

funVarsQ :: (HasSort a, Data b) => IdentMap a -> b -> IdentSet
funVarsQ symtab =
  everythingButFuns (<>)
  (const mempty `ext1Q` queryExpr)
  where
    queryExpr (VarE (ident -> x)) | Fun <- sort (symtab!x) =
      IdentSet.singleton x
    queryExpr _ =
      mempty
