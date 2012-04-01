{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
module Language.Glyph.Record.Stmts
       ( module X
       , Stmts
       , stmts
       ) where

import Data.Record as X
import Data.Typeable

import Text.PrettyPrint.Free

data Stmts deriving Typeable

stmts :: Stmts
stmts = undefined

instance Show Stmts where
  show = show . pretty

instance Pretty Stmts where
  pretty _ = text "stmts"
