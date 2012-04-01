{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.CallSet
       ( module X
       , CallSet
       , callSet
       ) where

import Data.Record as X
import Data.Typeable

import Text.PrettyPrint.Free

data CallSet deriving Typeable

callSet :: CallSet
callSet = undefined

instance Show CallSet where
  show = show . pretty

instance Pretty CallSet where
  pretty _ = text "callSet"
