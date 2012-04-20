{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.Types
       ( module X
       , Types
       , types
       ) where

import Data.Record as X
import Data.Typeable

import Language.Glyph.Pretty

data Types deriving Typeable

types :: Types
types = undefined

instance Show Types where
  show = showDefault

instance Pretty Types where
  pretty _ = text "types"
