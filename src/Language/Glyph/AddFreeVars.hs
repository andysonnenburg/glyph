{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Language.Glyph.AddFreeVars
       ( addFreeVars
       ) where

import Control.Applicative
import Control.Comonad
import Control.Monad.Reader

import Language.Glyph.Annotation.FreeVars hiding (freeVars)
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

addFreeVars :: ( HasSort a
              , MonadReader [Located (Stmt Ident)] m
              ) => IdentMap a -> m (IdentMap (With a FreeVars))
addFreeVars symtab = do
  freeVars <- asks $ freeVarsQ symtab
  return $
    IdentMap.intersectionWith withFreeVars symtab freeVars <>
    ((`withFreeVars` mempty) <$> symtab)

freeVarsQ :: (HasSort a, Data b) => IdentMap a -> b -> IdentMap IdentSet
freeVarsQ symtab =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt `extQ` queryExpr)
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
        nestedFreeVars = mconcat . map (freeVars!) . IdentSet.toList $ nestedFuns
        nestedFuns = funDeclsQ stmts
        freeVars = freeVarsQ symtab stmts

varDeclsQ :: Data a => a -> IdentSet
varDeclsQ =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt)
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
    queryExpr (VarE x) | Var <- sort (symtab!k) =
      IdentSet.singleton k
      where
        k = extract x
    queryExpr _ =
      mempty

funDeclsQ :: Data a => a -> IdentSet
funDeclsQ =
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

ident :: Located (Named Ident) -> Ident
ident = extract . extract
