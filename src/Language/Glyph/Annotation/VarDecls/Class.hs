module Language.Glyph.Annotation.VarDecls.Class
       ( VarDecls
       , HasVarDecls (..)
       ) where

import Language.Glyph.NameSet (NameSet)

type VarDecls = NameSet

class HasVarDecls a where
  varDecls :: a -> VarDecls