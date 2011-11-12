module Language.Glyph.Annotation.FreeVars.Class
       ( FreeVars
       , HasFreeVars (..)
       ) where

import Language.Glyph.IdentSet (IdentSet)

type FreeVars = IdentSet

class HasFreeVars a where
  freeVars :: a -> FreeVars