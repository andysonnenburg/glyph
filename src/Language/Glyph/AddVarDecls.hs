{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.AddVarDecls
       ( addVarDecls
       ) where

import Control.Comonad
import Control.Monad.Reader

import Data.Generics
import Data.Maybe

import Language.Glyph.Annotation.VarDecls
import Language.Glyph.Location
import Language.Glyph.NameMap (Name, NameMap)
import qualified Language.Glyph.NameMap as NameMap
import Language.Glyph.NameSet (NameSet)
import qualified Language.Glyph.NameSet as NameSet
import Language.Glyph.Syntax

addVarDecls :: ( MonadReader [Located (Stmt Name)] m
              ) => NameMap a -> m (NameMap (With a VarDecls))
addVarDecls symtab = do
  ss <- ask
  let varDecls' = varDeclsQ ss
      lookupVarDecls k = fromMaybe NameSet.empty (NameMap.lookup k varDecls')
      f k v = withVarDecls v (lookupVarDecls k)
  return $ NameMap.mapWithKey f symtab

varDeclsQ :: Data a => a -> NameMap NameSet
varDeclsQ =
  everything NameMap.union (NameMap.empty `mkQ`
                            queryStmt `extQ`
                            queryExpr)
  where
    queryStmt (FunDeclS x xs ss) =
      NameMap.singleton (name x) (NameSet.union (vars xs) (funsQ ss))
    queryStmt _ =
      NameMap.empty
    
    queryExpr (FunE x xs ss) =
      NameMap.singleton x (NameSet.union (vars xs) (funsQ ss))
    queryExpr _ =
      NameMap.empty

    vars = NameSet.fromList . map name

funsQ :: Data a => a -> NameSet
funsQ =
  everythingBut NameSet.union ((NameSet.empty, False) `mkQ` qS `ext1Q` qE)
  where
    qS (FunDeclS _ _ _) = (NameSet.empty, True)
    qS (VarDeclS x _) = (NameSet.singleton (name x), False)
    qS _ = (NameSet.empty, False)
    
    qE (FunE _ _ _) = (NameSet.empty, True)
    qE _ = (NameSet.empty, False)

name :: (Comonad w, Comonad v) => w (v a) -> a
name = extract . extract