{-# LANGUAGE EmptyDataDecls #-}
module Language.Glyph.Record.CallSet
       ( module X
       , CallSet
       , callSet
       ) where

import Data.Record as X

import Text.PrettyPrint.Free

data CallSet

callSet :: CallSet
callSet = undefined

instance Show CallSet where
  show = show . pretty

instance Pretty CallSet where
  pretty _ = text "callSet"
