{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.Name
       ( module X
       , Name
       , name
       ) where

import Data.Record as X
import Data.Typeable

import Text.PrettyPrint.Free

data Name deriving Typeable

name :: Name
name = undefined

instance Show Name where
  show = show . pretty

instance Pretty Name where
  pretty _ = text "name"