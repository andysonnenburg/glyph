{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Language.Glyph.Ident
       ( Ident
       , freshIdent
       ) where

import Compiler.Hoopl
import Compiler.Hoopl.GHC
import Control.Monad

import Data.Data
import Data.Hashable

import Language.Haskell.TH.Syntax (showName)
import qualified Language.Haskell.TH as TH

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
      name = $(return . TH.LitE . TH.StringL . showName $ ''Ident)

instance Hashable Ident where
  hash = uniqueToInt . unIdent

freshIdent :: UniqueMonad m => m Ident
freshIdent = liftM Ident freshUnique

