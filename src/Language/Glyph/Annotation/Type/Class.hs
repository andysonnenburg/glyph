module Language.Glyph.Annotation.Type.Class
       ( HasType (..)
       ) where

import Language.Glyph.Type

class HasType a where
  type' :: a -> Type
