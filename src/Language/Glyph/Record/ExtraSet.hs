{-# LANGUAGE EmptyDataDecls #-}
module Language.Glyph.Record.ExtraSet
       ( module X
       , ExtraSet
       , extraSet
       ) where

import Data.Record as X

import Text.PrettyPrint.Free

data ExtraSet

extraSet :: ExtraSet
extraSet = undefined

instance Show ExtraSet where
  show = show . pretty

instance Pretty ExtraSet where
  pretty _ = text "extraSet"