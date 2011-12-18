module Language.Glyph.HM.Token
       ( Token (..)
       ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Prelude hiding (True, False)

data Token
  = Backslash
  | Period
  | Equals
  | Let
  | In
  | True
  | False
  | Name Text
  | EOF

instance Show Token where
  show x =
    case x of
      Backslash -> "\\"
      Period -> "."
      Equals -> "="
      Let -> "let"
      In -> "in"
      True -> "true"
      False -> "false"
      Name a -> "`" ++ Text.unpack a ++ "'"
      EOF -> "end of file"