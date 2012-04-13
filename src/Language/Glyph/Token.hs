{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Glyph.Token
       ( Token (..)
       , loc
       , TokenView (..)
       ) where

import Data.Int
import Data.Text (Text)
import qualified Data.Text as Text

import Language.Glyph.Loc
import Language.Glyph.View

import Text.PrettyPrint.Free

import Prelude hiding (True, False)

data Token = Token Loc TokenView

instance Show Token where
  show = show . pretty

instance Pretty Token where
  pretty (Token p v) = pretty p <> char ':' <+> pretty v

data TokenView
  = Var
  | Fn
  | Name Text
  | LeftBrace
  | RightBrace
  | Colon
  | Semicolon
  | True
  | False
  | Int Int32
  | Double Double
  | String Text
  | Void
  | Return
  | If
  | Else
  | While
  | Break
  | Continue
  | Throw
  | Try
  | Finally
  | LeftParenthesis
  | RightParenthesis
  | Period
  | Bang
  | Plus
  | Minus
  | Equals
  | Assign
  | Comma
  | EOF

instance Show TokenView where
  show = show . pretty

instance Pretty TokenView where
  pretty x =
    case x of
      Var -> text "var"
      Fn -> text "fn"
      Name a -> char '`' <> text (Text.unpack a) <> char '\''
      LeftBrace -> char '{'
      RightBrace -> char '}'
      Colon -> char ':'
      Semicolon -> char ';'
      True -> text "true"
      False -> text "false"
      Int a -> pretty . toInteger $ a
      Double a -> pretty a
      String a -> dquotes . text . Text.unpack $ a
      Void -> text "void"
      Return -> text "return"
      If -> text "if"
      Else -> text "else"
      While -> text "while"
      Break -> text "break"
      Continue -> text "continue"
      Throw -> text "throw"
      Try -> text "try"
      Finally -> text "finally"
      LeftParenthesis -> char '('
      RightParenthesis -> char ')'
      Period -> char '.'
      Bang -> char '!'
      Plus -> char '+'
      Minus -> char '-'
      Equals -> text "=="
      Assign -> char '='
      Comma -> char ','
      EOF -> text "end" <+> text "of" <+> text "file"

loc :: Token -> Loc
loc (Token x _) = x

instance View Token TokenView where
  view (Token _ x) = x
