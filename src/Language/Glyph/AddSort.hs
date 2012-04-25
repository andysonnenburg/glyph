{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , ScopedTypeVariables
  , TypeOperators #-}
module Language.Glyph.AddSort
       ( addSort
       ) where

import Data.Generics
import Data.Monoid

import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Record hiding (Sort, Symtab, name)
import qualified Language.Glyph.Record as Record
import Language.Glyph.Sort
import Language.Glyph.Syntax

type Symtab sym = IdentMap (Record sym)
type Symtab' sym = IdentMap (Record ('(Record.Sort, Sort) ': sym))

addSort :: forall a sym m fields fields' .
           ( Data a
           , Select Stmts [Stmt a] fields
           , Select Record.Symtab (Symtab sym) fields
           , Remove Record.Symtab fields fields'
           , Lacks Record.Sort sym
           , Monad m
           ) =>
           Record fields ->
           m (Record ('(Record.Symtab, Symtab' sym) ': fields'))
addSort r = return $ symtab #= ((r#.symtab) `n` sorts) #| r #- symtab
  where
    sorts = sort' (r#.stmts)
    n = IdentMap.intersectionWith' (\ r' x -> sort #= x #| r') Var

    sort' :: Data a' => a' -> IdentMap Sort
    sort' =
      everything (<>)
      (const mempty `extQ` queryStmt `extQ` queryExpr)
      where
        queryStmt :: StmtView a -> IdentMap Sort
        queryStmt (FunDeclS name params _) =
          IdentMap.fromList $ fun name : vars params
        queryStmt (VarDeclS name _) =
          IdentMap.singleton (ident name) Var
        queryStmt _ =
          mempty

        queryExpr :: ExprView a -> IdentMap Sort
        queryExpr (FunE a params _) =
          IdentMap.fromList $ (a, Fun) : vars params
        queryExpr _ =
          mempty

        fun x = (ident x, Fun)
        vars xs = zip (map ident xs) (repeat Var)
