{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.Symtab
       ( module X
       , Symtab
       , symtab
       ) where

import Data.Record as X
import Data.Typeable

import Text.PrettyPrint.Free

data Symtab deriving Typeable

symtab :: Symtab
symtab = undefined

instance Show Symtab where
  show = show . pretty

instance Pretty Symtab where
  pretty _ = text "symtab"