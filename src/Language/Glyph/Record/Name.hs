{-# LANGUAGE EmptyDataDecls #-}
module Language.Glyph.Record.Name
       ( module X
       , Name
       , name
       ) where

import Data.Record as X

import Text.PrettyPrint.Free

data Name

name :: Name
name = undefined

instance Show Name where
  show = show . pretty

instance Pretty Name where
  pretty _ = text "name"