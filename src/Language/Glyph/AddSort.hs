{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Language.Glyph.AddSort
       ( Sort (..)
       , addSort
       ) where

import Control.Applicative
import Control.Exception

import Language.Glyph.Annotation.Location
import Language.Glyph.Annotation.Sort
import Language.Glyph.Generics
import Language.Glyph.Logger
import Language.Glyph.Monoid
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Syntax

addSort :: ( Data a
          , Monad m
          ) => ([Stmt a], IdentMap b) -> m ([Stmt a], IdentMap (Annotated Sort b))
addSort (stmts, symtab) = do
  let symtab' = sort' stmts
  return (stmts,
          IdentMap.intersectionWith (flip Annotated) symtab symtab' <>
          (throw PreviousErrors <$> symtab))

sort' :: Data a => a -> IdentMap Sort
sort' =
  everything (<>)
  (const mempty `ext1Q` queryStmt `ext1Q` queryExpr)
  where
    queryStmt (FunDeclS name params _) =
      IdentMap.fromList $ fun name:vars params
    queryStmt (VarDeclS name _) =
      IdentMap.singleton (ident name) Var
    queryStmt _ =
      mempty
    
    queryExpr (FunE a params _) =
      IdentMap.fromList $ (a, Fun):vars params
    queryExpr _ =
      mempty
    
    fun x = (ident x, Fun)
    vars xs = zip (map ident xs) (repeat Var)
