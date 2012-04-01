{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , PatternGuards
  , ScopedTypeVariables
  , TypeOperators
  , ViewPatterns #-}
module Language.Glyph.AddFreeVars
       ( addFreeVars
       ) where

import Data.Monoid

import Language.Glyph.Record hiding (Sort, Symtab)
import qualified Language.Glyph.Record as Record
import Language.Glyph.Sort
import Language.Glyph.Generics
import Language.Glyph.IdentMap (IdentMap, (!))
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Syntax

type Symtab sym = IdentMap (Record sym)
type Symtab' sym = IdentMap (Record ('(FreeVars, IdentSet) ': sym))

addFreeVars :: ( Data a
               , Select Record.Sort Sort sym
               , Select Stmts [Stmt a] fields
               , Select Record.Symtab (Symtab sym) fields
               , Remove Record.Symtab fields fields'
               , Lacks FreeVars sym
               , Monad m
               ) =>
               Record fields ->
               m (Record ('(Record.Symtab, Symtab' sym) ': fields'))
addFreeVars r = return $ symtab #= symtab'' #| (r #- symtab)
  where
    stmts' = r#.stmts
    symtab' = r#.symtab
    symtab'' =
      IdentMap.intersectionWith'
      (\ r' x -> freeVars #= x #| r') mempty symtab' freeVars'
    freeVars' = freeVarsQ symtab' stmts'

freeVarsQ :: forall a sym .
             ( Data a
             , Select Record.Sort Sort sym
             ) => IdentMap (Record sym) -> [Stmt a] -> IdentMap IdentSet
freeVarsQ symtab' =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt `extQ` queryExpr)
  where
    queryStmt :: StmtView a -> IdentMap IdentSet
    queryStmt (FunDeclS (ident -> x) params stmts') =
      queryFun x params stmts'
    queryStmt _ =
      mempty

    queryExpr :: ExprView a -> IdentMap IdentSet
    queryExpr (FunE x params stmts') =
      queryFun x params stmts'
    queryExpr _ =
      mempty

    queryFun x (map ident -> params) stmts' =
      freeVars' <>
      IdentMap.singleton x (nestedFreeVars <> vars \\ varDecls)
      where
        varDecls = varDeclsQ stmts' <> IdentSet.fromList params
        vars = varsQ symtab' stmts'
        nestedFreeVars =
          mconcat .
          map (freeVars' !) .
          IdentSet.toList $
          nestedFuns
        nestedFuns = funDeclsQ stmts'
        freeVars' = freeVarsQ symtab' stmts'

varDeclsQ :: forall a . Data a => [Stmt a] -> IdentSet
varDeclsQ =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt)
  where
    queryStmt :: StmtView a -> IdentSet
    queryStmt (VarDeclS x _) =
      IdentSet.singleton (ident x)
    queryStmt _ =
      mempty

varsQ :: ( Data a
         , Select Record.Sort Sort sym
         ) => IdentMap (Record sym) -> a -> IdentSet
varsQ symtab' =
  everythingButFuns (<>)
  (mempty `mkQ` queryExpr)
  where
    queryExpr x | Var <- (symtab' ! x)#.sort =
      IdentSet.singleton x
    queryExpr _ =
      mempty

funDeclsQ :: forall a . Data a => [Stmt a] -> IdentSet
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
