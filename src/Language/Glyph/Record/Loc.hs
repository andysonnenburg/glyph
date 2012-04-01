{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.Loc
       ( module X
       , Loc
       , loc
       ) where

import Data.Record as X
import Data.Typeable

import Text.PrettyPrint.Free

data Loc deriving Typeable

loc :: Loc
loc = undefined

instance Show Loc where
  show = show . pretty

instance Pretty Loc where
  pretty _ = text "loc"
