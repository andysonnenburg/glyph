module Language.Glyph.Annotation.Sort.Class
       ( Sort (..)
       , HasSort (..)
       ) where

data Sort = Var | Fun deriving (Show, Eq)

class HasSort a where
  sort :: a -> Sort
