{-# LANGUAGE DeriveDataTypeable #-}
module Language.Glyph.Ident
       ( Ident
       , freshIdent
       ) where

import Compiler.Hoopl
import Compiler.Hoopl.GHC
import Control.DeepSeq
import Control.Monad

import Data.Data
import Data.Hashable

import Text.PrettyPrint.Free

newtype Ident
  = Ident { unIdent :: Unique
          } deriving (Eq, Ord, Typeable)

instance Show Ident where
  show = show . pretty

instance Pretty Ident where
  pretty = text . ('$' :) . show . unIdent

instance Data Ident where
  gfoldl _f z = z
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType name
    where
      name = "Language.Glyph.Ident.Ident"

instance Hashable Ident where
  hash = uniqueToInt . unIdent

instance NFData Ident

freshIdent :: UniqueMonad m => m Ident
freshIdent = liftM Ident freshUnique

