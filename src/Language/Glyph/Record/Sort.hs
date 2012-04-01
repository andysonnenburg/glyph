{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.Sort
       ( module X
       , Sort
       , sort
       ) where

import Data.Record as X
import Data.Typeable

import Text.PrettyPrint.Free

data Sort deriving Typeable

sort :: Sort
sort = undefined

instance Show Sort where
  show = show . pretty

instance Pretty Sort where
  pretty _ = text "sort"


