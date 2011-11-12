module Language.Glyph.Token
       ( Token (..)
       ) where

import Data.Int
import Data.Text (Text)
import qualified Data.Text as Text

import Prelude hiding (Bool (..))

data Token
  = Var
  | Fun
  | Name Text
  | Period
  | Comma
  | LeftParenthesis
  | RightParenthesis
  | LeftBrace
  | RightBrace
  | Colon
  | Semicolon
  | True
  | False
  | Return
  | Equals
  | EOF

instance Show Token where
  show x =
    case x of
      Var -> "var"
      Fun -> "fun"
      Name a -> "`" ++ Text.unpack a ++ "'"
      Period -> "."
      Comma -> ","
      LeftParenthesis -> "("
      RightParenthesis -> ")"
      LeftBrace -> "{"
      RightBrace -> "}"
      Colon -> ":"
      Semicolon -> ";"
      True -> "true"
      False -> "false"
      Return -> "return"
      Equals -> "="
      EOF -> "end of file"