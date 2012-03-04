module Language.Glyph.Annotation.TypeScheme.Class
       ( HasTypeScheme (..)
       ) where

import Language.Glyph.Type

class HasTypeScheme a where
  typeScheme :: a -> TypeScheme
