{-# LANGUAGE FlexibleContexts, PatternGuards, ScopedTypeVariables, ViewPatterns #-}
module Language.Glyph.AddFreeVars
       ( addFreeVars
       ) where

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
addFreeVars (stmts, symtab) =
  return (stmts, IdentMap.unionWith' (flip withFreeVars) mempty symtab symtab')
  where
    symtab' = freeVarsQ symtab stmts

freeVarsQ :: forall a b.
            ( HasSort a
            , Data b
            ) => IdentMap a -> [Stmt b] -> IdentMap IdentSet
freeVarsQ symtab =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt `extQ` queryExpr)
  where
    queryStmt :: StmtView b -> IdentMap IdentSet
    queryStmt (FunDeclS (ident -> x) params stmts) =
      queryFun x params stmts
    queryStmt _ =
      mempty
    
    queryExpr :: ExprView b -> IdentMap IdentSet
    queryExpr (FunE x params stmts) =
      queryFun x params stmts
    queryExpr _ =
      mempty
    
    queryFun x (map ident -> params) stmts =
      freeVars <>
      IdentMap.singleton x (nestedFreeVars <> vars \\ varDecls)
      where
        varDecls = varDeclsQ stmts <> IdentSet.fromList params
        vars = varsQ symtab stmts
        nestedFreeVars =
          mconcat .
          map (freeVars!) .
          IdentSet.toList $
          nestedFuns
        nestedFuns = funDeclsQ stmts
        freeVars = freeVarsQ symtab stmts

varDeclsQ :: forall a. Data a => [Stmt a] -> IdentSet
varDeclsQ =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt)
  where
    queryStmt :: StmtView a -> IdentSet
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

funDeclsQ :: forall a. Data a => [Stmt a] -> IdentSet
funDeclsQ =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt `extQ` queryExpr)
  where
    queryStmt :: StmtView a -> IdentSet
    queryStmt (FunDeclS (ident -> x) _ _) =
      IdentSet.singleton x
    queryStmt _ =
      mempty
    
    queryExpr :: ExprView a -> IdentSet
    queryExpr (FunE x _ _) =
      IdentSet.singleton x
    queryExpr _ =
      mempty
