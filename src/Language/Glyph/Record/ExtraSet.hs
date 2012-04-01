{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.ExtraSet
       ( module X
       , ExtraSet
       , extraSet
       ) where

import Data.Record as X
import Data.Typeable

import Text.PrettyPrint.Free

data ExtraSet deriving Typeable

extraSet :: ExtraSet
extraSet = undefined

instance Show ExtraSet where
  show = show . pretty

instance Pretty ExtraSet where
  pretty _ = text "extraSet"