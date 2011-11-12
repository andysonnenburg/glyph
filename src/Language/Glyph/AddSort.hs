{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.AddSort
       ( Sort (..)
       , addSort
       ) where

import Control.Comonad
import Control.Monad.Reader

import Language.Glyph.Annotation.Sort
import Language.Glyph.Generics
import Language.Glyph.Location
import Language.Glyph.Monoid
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Syntax

addSort :: MonadReader [Located (Stmt Ident)] m =>
          IdentMap a ->
          m (IdentMap (With a Sort))
addSort symtab = asks $ IdentMap.intersectionWith With symtab . sortQ

sortQ :: Data a => a -> IdentMap Sort
sortQ =
  everything (<>)
  (mempty `mkQ` queryStmt `extQ` queryExpr)
  where
    queryStmt (FunDeclS x xs _) = IdentMap.fromList $ [fun x] ++ vars xs
    queryStmt (VarDeclS x _) = IdentMap.singleton (ident x) Var
    queryStmt _ = mempty
    
    queryExpr (FunE x xs _) = IdentMap.fromList $ [(x, Fun)] ++ vars xs
    queryExpr _ = mempty
    
    fun x = (ident x, Fun)
    vars xs = zip (map ident xs) (repeat Var)
    ident = extract . extract
