module Language.Glyph.Sort ( Sort (..)) where

import Text.PrettyPrint.Free

data Sort = Var | Fun

instance Show Sort where
  show = show . pretty

instance Pretty Sort where
  pretty = go
    where
      go Var = text "var"
      go Fun = text "fun"