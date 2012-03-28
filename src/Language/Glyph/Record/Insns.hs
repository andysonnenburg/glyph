{-# LANGUAGE EmptyDataDecls #-}
module Language.Glyph.Record.Insns
       ( module X
       , Insns
       , insns
       ) where

import Data.Record as X

import Text.PrettyPrint.Free

data Insns

insns :: Insns
insns = undefined

instance Show Insns where
  show = show . pretty

instance Pretty Insns where
  pretty _ = text "insns"