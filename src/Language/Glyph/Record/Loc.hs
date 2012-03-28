{-# LANGUAGE EmptyDataDecls #-}
module Language.Glyph.Record.Loc
       ( module X
       , Loc
       , loc
       ) where

import Data.Record as X

import Text.PrettyPrint.Free

data Loc

loc :: Loc
loc = undefined

instance Show Loc where
  show = show . pretty

instance Pretty Loc where
  pretty _ = text "loc"
