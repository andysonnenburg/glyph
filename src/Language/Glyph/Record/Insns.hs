{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.Insns
       ( module X
       , Insns
       , insns
       ) where

import Data.Record as X
import Data.Typeable

import Text.PrettyPrint.Free

data Insns deriving Typeable

insns :: Insns
insns = undefined

instance Show Insns where
  show = show . pretty

instance Pretty Insns where
  pretty _ = text "insns"