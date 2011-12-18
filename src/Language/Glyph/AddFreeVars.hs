{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Language.Glyph.AddFreeVars
       ( addFreeVars
       ) where

import Control.Applicative

import Language.Glyph.Annotation.FreeVars hiding (freeVars)
import Language.Glyph.Annotation.Sort
import Language.Glyph.Generics
import Language.Glyph.Monoid
import Language.Glyph.IdentMap (IdentMap, (!))
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Syntax

addFreeVars :: ( Data a
              , HasSort b
              , Monad m
              ) => ([Stmt a], IdentMap b) -> m ([Stmt a], IdentMap (Annotated FreeVars b))
addFreeVars (stmts, symtab) = do
  let symtab' = freeVarsQ symtab stmts
  return (stmts,
          IdentMap.intersectionWith (flip withFreeVars) symtab symtab' <>
          (withFreeVars mempty <$> symtab))

freeVarsQ :: (HasSort a, Data b) => IdentMap a -> b -> IdentMap IdentSet
freeVarsQ symtab =
  everythingButFuns (<>)
  (const mempty `ext1Q` queryStmt `ext1Q` queryExpr)
  where
    queryStmt (FunDeclS x params stmts) =
      queryFun (ident x) params stmts
    queryStmt _ =
      mempty
    
    queryExpr (FunE x params stmts) =
      queryFun x params stmts
    queryExpr _ =
      mempty
    
    queryFun x params stmts =
      freeVars <>
      IdentMap.singleton x (nestedFreeVars <> vars \\ declaredVars)
      where
        declaredVars = varDecls <> IdentSet.fromList (map ident params)
        varDecls = varDeclsQ stmts
        vars = varsQ symtab stmts
        nestedFreeVars =
          mconcat .
          map (freeVars!) .
          IdentSet.toList $
          nestedFuns
        nestedFuns = funDeclsQ stmts
        freeVars = freeVarsQ symtab stmts

varDeclsQ :: Data a => a -> IdentSet
varDeclsQ =
  everythingButFuns (<>)
  (const mempty `ext1Q` queryStmt)
  where
    queryStmt (VarDeclS x _) =
      IdentSet.singleton (ident x)
    queryStmt _ =
      mempty

varsQ :: (HasSort a, Data b) => IdentMap a -> b -> IdentSet
varsQ symtab =
  everythingButFuns (<>)
  (mempty `mkQ` queryExpr)
  where  
    queryExpr x | Var <- sort (symtab!x) =
      IdentSet.singleton x
    queryExpr _ =
      mempty

funDeclsQ :: Data a => a -> IdentSet
funDeclsQ =
  everythingButFuns (<>)
  (const mempty `ext1Q` queryStmt `ext1Q` queryExpr)
  where
    queryStmt (FunDeclS x _ _) =
      IdentSet.singleton (ident x)
    queryStmt _ =
      mempty
    
    queryExpr (FunE x _ _) =
      IdentSet.singleton x
    queryExpr _ =
      mempty
