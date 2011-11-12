{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Language.Glyph.Internal.Ident
       ( Ident (..)
       ) where

import Data.Data

import Language.Haskell.TH.Syntax (showName)
import qualified Language.Haskell.TH as TH

newtype Ident
  = Ident { unIdent :: Int
          } deriving (Show, Eq, Ord, Typeable)

instance Data Ident where
  gfoldl _f z n = z n
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType name
    where
      name = $(return . TH.LitE . TH.StringL . showName $ ''Ident)