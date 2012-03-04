module Language.Glyph.Annotation.CallSet.Class
       ( CallSet
       , HasCallSet (..)
       ) where

import Language.Glyph.IdentSet (IdentSet)

type CallSet = IdentSet

class HasCallSet a where
  callSet :: a -> CallSet
