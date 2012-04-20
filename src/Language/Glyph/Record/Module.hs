{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.Module
       ( module X
       , Module
       , module'
       ) where

import Data.Record as X
import Data.Typeable

import Language.Glyph.Pretty

data Module deriving Typeable

module' :: Module
module' = undefined

instance Show Module where
  show = showDefault

instance Pretty Module where
  pretty _ = text "module"
