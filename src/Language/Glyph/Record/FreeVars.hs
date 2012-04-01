{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.FreeVars
       ( module X
       , FreeVars
       , freeVars
       ) where

import Data.Record as X
import Data.Typeable

import Text.PrettyPrint.Free

data FreeVars deriving Typeable

freeVars :: FreeVars
freeVars = undefined

instance Show FreeVars where
  show = show . pretty

instance Pretty FreeVars where
  pretty _ = text "freeVars"