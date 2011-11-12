module Language.Glyph.TypeVarMap
       ( TypeVarMap
       ) where

import Data.IntMap

newtype TypeVarMap v
  = TypeVarMap { unTypeVarMap :: IntMap v
               }