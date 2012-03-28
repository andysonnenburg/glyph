{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Glyph.Token
       ( Token (..)
       , loc
       , TokenView (..)
       ) where

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

instance Show TokenView where
  show = show . pretty

instance Pretty TokenView where
  pretty x =
    case x of
      Var -> text "var"
      Fn -> text "fn"
      Name a -> char '`' <> text (Text.unpack a) <> char '\''
      Period -> char '.'
      Comma -> char ','
      LeftParenthesis -> char '('
      RightParenthesis -> char ')'
      LeftBrace -> char '{'
      RightBrace -> char '}'
      Colon -> char ':'
      Semicolon -> char ';'
      Int a -> pretty a
      True -> text "true"
      False -> text "false"
      Void -> text "void"
      Bang -> char '!'
      Return -> text "return"
      If -> text "if"
      Else -> text "else"
      While -> text "while"
      Break -> text "break"
      Continue -> text "continue"
      Throw -> text "throw"
      Try -> text "try"
      Finally -> text "finally"
      Equals -> char '='
      EOF -> text "end" <+> text "of" <+> text "file"

loc :: Token -> Loc
loc (Token x _) = x

instance View Token TokenView where
  view (Token _ x) = x