{-# LANGUAGE
    FlexibleContexts
  , PatternGuards
  , ScopedTypeVariables
  , TypeOperators
  , ViewPatterns #-}
module Language.Glyph.AddCallSet
       ( addCallSet
       ) where

import Data.Monoid

import Language.Glyph.Generics
import Language.Glyph.IdentMap (IdentMap, (!))
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Record hiding (Sort, Symtab)
import qualified Language.Glyph.Record as Record
import Language.Glyph.Sort
import Language.Glyph.Syntax

type Symtab sym = IdentMap (Record sym)
type Symtab' sym = IdentMap (Record ('(CallSet, IdentSet) ': sym))

addCallSet :: ( Data a
              , Select Record.Sort Sort sym
              , Select Stmts [Stmt a] fields
              , Select Record.Symtab (Symtab sym) fields
              , Remove Record.Symtab fields fields'
              , Monad m
              ) =>
              Record fields ->
              m (Record ('(Record.Symtab, Symtab' sym) ': fields'))
addCallSet r = return $ symtab #= symtab'' #| (r #- symtab)
  where
    stmts' = r#.stmts
    symtab' = r#.symtab
    symtab'' =
      IdentMap.intersectionWith'
      (\ r' x -> callSet #= x #| r') mempty symtab' callSets
    callSets = callSetsQ symtab' stmts'

callSetsQ :: forall a sym .
             ( Data a
             , Select Record.Sort Sort sym
             ) => IdentMap (Record sym) -> [Stmt a] -> IdentMap IdentSet
callSetsQ symtab' =
  everythingButFuns (<>)
  (mempty `mkQ` queryStmt `extQ` queryExpr)
  where
    queryStmt :: StmtView a -> IdentMap IdentSet
    queryStmt (FunDeclS (ident -> x) _ stmts') =
      queryFun x stmts'
    queryStmt _ =
      mempty

    queryExpr :: ExprView a -> IdentMap IdentSet
    queryExpr (FunE x _ stmts') =
      queryFun x stmts'
    queryExpr _ =
      mempty

    queryFun x stmts' =
      callSets <> IdentMap.singleton x callSet'
      where
        callSet' = nestedCallSet <> funVars \\ nestedFuns
        nestedCallSet =
          mconcat .
          map (callSets !) .
          IdentSet.toList $
          nestedFuns
        funVars = funVarsQ symtab' stmts'
        nestedFuns = nestedFunsQ stmts'
        callSets = callSetsQ symtab' stmts'

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
            , Select Record.Sort Sort sym
            ) => IdentMap (Record sym) -> [Stmt a] -> IdentSet
funVarsQ symtab' =
  everythingButFuns (<>)
  (mempty `mkQ` queryExpr)
  where
    queryExpr :: ExprView a -> IdentSet
    queryExpr (VarE (ident -> x)) | Fun <- (symtab' ! x)#.sort =
      IdentSet.singleton x
    queryExpr _ =
      mempty
