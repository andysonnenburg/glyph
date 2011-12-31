{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.AddName
       ( addName
       ) where

import Control.Applicative

import Data.Generics

import Language.Glyph.Annotation.Location
import Language.Glyph.Annotation.Name
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Monoid
import Language.Glyph.Syntax hiding (Name)

addName :: ( Data a
          , Monad m
          ) => ([Stmt a], IdentMap b) -> m ([Stmt a], IdentMap (Annotated Name b))
addName (stmts, symtab) = do
  let symtab' = name' stmts
  return (stmts,
          IdentMap.intersectionWith (flip withName) symtab symtab' <>
          (withName Nothing <$> symtab))

name' :: Data a => a -> IdentMap (Maybe NameView)
name' =
  everything (<>)
  (mempty `mkQ` queryName)
  where
    queryName x =
      IdentMap.singleton (ident x) (Just . view $ x)
