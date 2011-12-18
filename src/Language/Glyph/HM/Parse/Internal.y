{
{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}
module Language.Glyph.HM.Parse.Internal
       ( parse
       ) where

import Control.Comonad
import Control.Monad.Error

import Data.Functor.Apply

import Language.Glyph.Annotation
import Language.Glyph.Annotation.Location
import Language.Glyph.Parser.Internal
import Language.Glyph.HM.Syntax hiding (Expr, Named, name)
import qualified Language.Glyph.HM.Syntax as Syntax
import Language.Glyph.HM.Lex.Internal
import Language.Glyph.HM.Token hiding (False, True)
import qualified Language.Glyph.HM.Token as Token

import Prelude hiding (lex)
}

%tokentype { Annotated Location Token }

%token
'\\' { (extract -> Backslash) }
'.' { (extract -> Period) }
'=' { (extract -> Equals) }
NAME { (extract -> Name _) }

%name parse

%monad { MonadError ParseException m } { ParserT m } { >>= } { return }

%lexer { lexer } { (extract -> EOF) }

%error { parseError }

%%

expr :: { Annotated Location Expr }
  : var { $1 }

var :: { Annotated Location Expr }
  : named { var <%> $1 }

named :: { Annotated Location Named }
  : NAME { named <%> $1 }

{
type Expr = Syntax.Expr Location ()
type Named = Syntax.Named ()

lexer :: MonadError ParseException m =>
        (Annotated Location Token -> ParserT m a) ->
        ParserT m a  
lexer = (lex >>=)

parseError :: ( MonadError ParseException m
             , Comonad w
             , HasLocation (w Token)
             ) =>
             w Token ->
             ParserT m a
parseError x = throwError $ ParseError (location x) ("unexpected " ++ show x')
  where
    x' = extract x

var :: Named -> Expr
var = Var

named :: Token -> Named
named (Name x) = Syntax.Named x ()

infixl 4 <%>
(<%>) :: Functor f => (a -> b) -> f a -> f b
(<%>) = (<$>)

infixl 4 <%
(<%) :: Functor f => a -> f b -> f a
(<%) = (<$)
}