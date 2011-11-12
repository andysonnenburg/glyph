{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Language.Glyph.AddCallSet
       ( addCallSet
       ) where

import Control.Applicative
import Control.Comonad
import Control.Monad.Reader

import Language.Glyph.Annotation.CallSet hiding (callSet)
import Language.Glyph.Annotation.Sort
import Language.Glyph.Generics
import Language.Glyph.Location
import Language.Glyph.Monoid
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Syntax

addCallSet :: ( HasSort a
              , MonadReader [Located (Stmt Ident)] m
              ) => IdentMap a -> m (IdentMap (With a CallSet))
addCallSet symtab = do
  callSets <- asks $ callSetsQ symtab
  return $
    IdentMap.intersectionWith withCallSet symtab callSets <>
    ((`withCallSet` mempty) <$> symtab)

callSetsQ :: (HasSort a, Data b) => IdentMap a -> b -> IdentMap IdentSet
callSetsQ symtab =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt `extQ` queryExpr)
  where
    queryStmt (FunDeclS x _ stmts) =
      queryFun (ident x) stmts
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
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt `extQ` queryExpr)
  where
    queryStmt (FunDeclS x _ _) =
      IdentSet.singleton (ident x)
    queryStmt _ = 
      mempty
    
    queryExpr (FunE x _ _) =
      IdentSet.singleton x
    queryExpr _ =
      mempty

funVarsQ :: (HasSort a, Data b) => IdentMap a -> b -> IdentSet
funVarsQ symtab =
  everythingButFuns (<>)
  (mempty `mkQ` queryExpr)
  where
    queryExpr (VarE x) | Fun <- sort (symtab!k) =
      IdentSet.singleton k
      where
        k = extract x
    queryExpr _ =
      mempty

ident :: Located (Named Ident) -> Ident
ident = extract . extract
        