{-# LANGUAGE EmptyDataDecls #-}
module Language.Glyph.Record.FreeVars
       ( module X
       , FreeVars
       , freeVars
       ) where

import Data.Record as X

import Text.PrettyPrint.Free

data FreeVars

freeVars :: FreeVars
freeVars = undefined

instance Show FreeVars where
  show = show . pretty

instance Pretty FreeVars where
  pretty _ = text "freeVars"