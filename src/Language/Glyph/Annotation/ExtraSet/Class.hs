module Language.Glyph.Annotation.ExtraSet.Class
       ( ExtraSet
       , HasExtraSet (..)
       ) where

import Language.Glyph.IdentSet (IdentSet)

type ExtraSet = IdentSet

class HasExtraSet a where
  extraSet :: a -> ExtraSet