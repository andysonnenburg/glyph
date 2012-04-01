{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , TypeOperators #-}
module Language.Glyph.AddName
       ( addName
       ) where

import Data.Generics
import Data.Monoid

import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Record hiding (Symtab)
import qualified Language.Glyph.Record as Record
import Language.Glyph.Syntax hiding (Name)

type Symtab sym = IdentMap (Record sym)
type Symtab' sym = IdentMap (Record ('(Name, Maybe NameView) ': sym))

addName :: ( Data a
           , Select Stmts [Stmt a] fields
           , Select Record.Symtab (Symtab sym) fields
           , Remove Record.Symtab fields fields'
           , Lacks Name sym
           , Monad m
           ) => 
           Record fields ->
           m (Record ('(Record.Symtab, Symtab' sym) ': fields'))
addName r = return $ symtab #= symtab'' #| (r #- symtab)
  where
    stmts' = r#.stmts
    symtab' = r#.symtab
    symtab'' =
      IdentMap.intersectionWith'
      (\ r' x -> name #= x #| r') mempty symtab' names'
    names' = names stmts'

names :: Data a => a -> IdentMap (Maybe NameView)
names =
  everything (<>)
  (mempty `mkQ` queryName)
  where
    queryName x =
      IdentMap.singleton (ident x) (Just . view $ x)
