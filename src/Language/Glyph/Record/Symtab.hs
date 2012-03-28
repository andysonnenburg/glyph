{-# LANGUAGE EmptyDataDecls #-}
module Language.Glyph.Record.Symtab
       ( module X
       , Symtab
       , symtab
       ) where

import Data.Record as X

import Text.PrettyPrint.Free

data Symtab

symtab :: Symtab
symtab = undefined

instance Show Symtab where
  show = show . pretty

instance Pretty Symtab where
  pretty _ = text "symtab"