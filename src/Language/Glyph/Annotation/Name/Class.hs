module Language.Glyph.Annotation.Name.Class
       ( Name
       , HasName (..)
       ) where

import Data.Text (Text)

type Name = Maybe Text

class HasName a where
  name :: a -> Name