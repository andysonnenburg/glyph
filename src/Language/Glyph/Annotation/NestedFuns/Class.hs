module Language.Glyph.Annotation.NestedFuns.Class
       ( NestedFuns
       , HasNestedFuns (..)
       ) where

import Language.Glyph.NameSet (NameSet)

type NestedFuns = NameSet

class HasNestedFuns a where
  nestedFuns :: a -> NestedFuns
