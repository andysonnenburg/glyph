module Language.Glyph.Token
       ( Token (..)
       ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Prelude hiding (True, False)

data Token
  = Var
  | Fn
  | Name Text
  | Period
  | Comma
  | LeftParenthesis
  | RightParenthesis
  | LeftBrace
  | RightBrace
  | Colon
  | Semicolon
  | Int Integer
  | True
  | False
  | Void
  | Bang
  | Return
  | If
  | Else
  | While
  | Break
  | Continue
  | Throw
  | Try
  | Finally
  | Equals
  | EOF

instance Show Token where
  show x =
    case x of
      Var -> "var"
      Fn -> "fn"
      Name a -> "`" ++ Text.unpack a ++ "'"
      Period -> "."
      Comma -> ","
      LeftParenthesis -> "("
      RightParenthesis -> ")"
      LeftBrace -> "{"
      RightBrace -> "}"
      Colon -> ":"
      Semicolon -> ";"
      Int a -> show a
      True -> "true"
      False -> "false"
      Void -> "void"
      Bang -> "!"
      Return -> "return"
      If -> "if"
      Else -> "else"
      While -> "while"
      Break -> "break"
      Continue -> "continue"
      Throw -> "throw"
      Try -> "try"
      Finally -> "finally"
      Equals -> "="
      EOF -> "end of file"
