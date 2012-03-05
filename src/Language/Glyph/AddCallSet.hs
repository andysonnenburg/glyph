{-# LANGUAGE
    FlexibleContexts
  , PatternGuards
  , ScopedTypeVariables
  , ViewPatterns #-}
module Language.Glyph.AddCallSet
       ( addCallSet
       ) where

import Data.Monoid

import Language.Glyph.Annotation.CallSet hiding (callSet)
import Language.Glyph.Annotation.Sort
import Language.Glyph.Generics
import Language.Glyph.IdentMap (IdentMap, (!), intersectionWith')
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Syntax

addCallSet :: ( Data a
             , HasSort sym
             , Monad m
             ) =>
             ([Stmt a], IdentMap sym) ->
             m ([Stmt a], IdentMap (Annotated CallSet sym))
addCallSet (stmts, symtab) =
  return (stmts, intersectionWith' (flip withCallSet) mempty symtab symtab')
  where
    symtab' = callSetsQ symtab stmts

callSetsQ :: forall a sym .
             ( Data a
             , HasSort sym
             ) => IdentMap sym -> [Stmt a] -> IdentMap IdentSet
callSetsQ symtab =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt `extQ` queryExpr)
  where
    queryStmt :: StmtView a -> IdentMap IdentSet
    queryStmt (FunDeclS (ident -> x) _ stmts) =
      queryFun x stmts
    queryStmt _ =
      mempty

    queryExpr :: ExprView a -> IdentMap IdentSet
    queryExpr (FunE x _ stmts) =
      queryFun x stmts
    queryExpr _ =
      mempty

    queryFun x stmts =
      callSets <> IdentMap.singleton x callSet
      where
        callSet = nestedCallSet <> funVars \\ nestedFuns
        nestedCallSet =
          mconcat .
          map (callSets !) .
          IdentSet.toList $
          nestedFuns
        funVars = funVarsQ symtab stmts
        nestedFuns = nestedFunsQ stmts
        callSets = callSetsQ symtab stmts

nestedFunsQ :: forall a . Data a => [Stmt a] -> IdentSet
nestedFunsQ =
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

funVarsQ :: forall a sym .
            ( Data a
            , HasSort sym
            ) => IdentMap sym -> [Stmt a] -> IdentSet
funVarsQ symtab =
  everythingButFuns (<>)
  (mempty `mkQ` queryExpr)
  where
    queryExpr :: ExprView a -> IdentSet
    queryExpr (VarE (ident -> x)) | Fun <- sort (symtab !x) =
      IdentSet.singleton x
    queryExpr _ =
      mempty
