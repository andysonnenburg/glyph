module Language.Glyph.Token
       ( Token (..)
       ) where

import Data.Int
import Data.Text (Text)
import qualified Data.Text as Text

import Prelude hiding (True, False)

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