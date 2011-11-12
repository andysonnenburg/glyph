{
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fspec-constr-count=17 #-}
module Language.Glyph.Internal.Parse
       ( parse
       ) where

import Control.Comonad
import Control.Monad.Error
import Control.Monad.Identity

import Data.Functor.Apply
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)

import Language.Glyph.Internal.Lex
import Language.Glyph.Internal.Location
import Language.Glyph.Internal.Parser
import Language.Glyph.Syntax hiding (Expr, Stmt, Named, name)
import qualified Language.Glyph.Syntax as Syntax
import Language.Glyph.Token hiding (False, Fun, Return, True, Var)
import qualified Language.Glyph.Token as Token

import Prelude hiding (lex)
}

%tokentype { Located Token }

%token
VAR { Located _ Token.Var }
FUN { Located _ Token.Fun }
NAME { Located _ (Name _) }
'.' { Located _ Period }
',' { Located _ Comma }
'(' { Located _ LeftParenthesis }
')' { Located _ RightParenthesis }
'{' { Located _ LeftBrace }
'}' { Located _ RightBrace }
';' { Located _ Semicolon }
':' { Located _ Colon }
FALSE { Located _ Token.True }
TRUE { Located _ Token.False }
RETURN { Located _ Token.Return }
'=' { Located _ Equals }

%name parse

%monad { MonadError ParseException m } { ParserT m } { >>= } { return }

%lexer { lexer } { Located _ EOF }

%error { parseError }

%left ','
%right '='
%left '(' ')'

%%

top :: { [Located Stmt] }
  : manyStmts { $1 }

manyStmts :: { [Located Stmt] }
  : many(stmt) { $1 }

stmt :: { Located Stmt }
  : simple ';' { $1 <. $2 }
  | composite { $1 }

simple :: { Located Stmt }
  : stmtExpr { ExprS $1 <% $1 }
  | varDecl { $1 }
  | return { $1 }

composite :: { Located Stmt }
  : funDecl { $1 }
  | block { $1 }

stmtExpr :: { Located Expr }
  : var { $1 }
  | fun { $1 }
  | bool { $1 }
  | apply { $1 }
  | assign { $1 }
  | '(' expr ')' { $2 }

expr :: { Located Expr }
  : stmtExpr { $1 }

return :: { Located Stmt }
  : RETURN optionMaybe(expr) { (maybe id (.>) $2) (return' $2 <% $1) }

varDecl :: { Located Stmt }
  : VAR named optionMaybe(snd('=', expr)) {
      (maybe (<. $2) (.>) $3) (varDecl $2 $3 <% $1)
    }

funDecl :: { Located Stmt }
  : FUN named '(' parameters ')' '{' manyStmts '}' {
      funDecl $2 $4 $7 <% $1 <. $8
    }

block :: { Located Stmt }
  : '{' manyStmts '}' { block $2 <% $1 <. $3 }

var :: { Located Expr }
  : named { var <%> $1 }

fun :: { Located Expr }
  : FUN '(' parameters ')' '{' manyStmts '}' { fun $3 $6 <% $1 <. $7 }

obj :: { Located Expr }
  : '{' properties '}' { obj $2 <% $1 <. $3 }

bool :: { Located Expr }
  : TRUE { bool True <% $1 }
  | FALSE { bool False <% $1 }

apply :: { Located Expr }
  : expr '(' arguments ')' { apply $1 $3 <% $1 <. $4 }

assign :: { Located Expr }
  : named '=' expr { assign $1 $3 <% $1 <. $3 }

parameters :: { [Located Named] }
  : sepBy(named, ',') { $1 }

properties :: { [(Located Text, Located Expr)] }
  : sepBy(property, ',') { $1 }

property :: { (Located Text, Located Expr) }
  : name ':' expr { ($1, $3) }

name :: { Located Text }
  : NAME { name <%> $1 }

arguments :: { [Located Expr] }
  : sepBy(expr, ',') { $1 }

named :: { Located Named }
  : NAME { named <%> $1 }

option(x, p) :: { a }
  : { x }
  | p { $1 }

optionMaybe(p) :: { Maybe a }
  : { Nothing }
  | p { Just $1 }

many(p) :: { [a] }
  : manyReversed(p) { reverse $1 }

manyReversed(p) :: { [a] }
  : { [] }
  | manyReversed(p) p { $2 : $1 }

sepBy(p, sep) :: { [a] }
  : { [] }
  | sepBy1(p, sep) { $1 }

sepBy1(p, sep) :: { [a] }
  : p many(snd(sep, p)) { $1 : $2 }

snd(p, q) :: { a }
  : p q { $2 }
{
type Stmt = Syntax.Stmt ()
type Expr = Syntax.Expr ()
type Named = Syntax.Named ()

lexer :: MonadError ParseException m =>
        (Located Token -> ParserT m a) ->
        ParserT m a  
lexer = (lex >>=)

parseError :: MonadError ParseException m => Located Token -> ParserT m a
parseError x = throwError $ ParseError (location x) ("unexpected " ++ show x')
  where
    x' = extract x

varDecl :: Located Named -> Maybe (Located Expr) -> Stmt
varDecl = VarDeclS

funDecl :: Located Named -> [Located Named] -> [Located Stmt] -> Stmt
funDecl = FunDeclS

block :: [Located Stmt] -> Stmt
block = BlockS

return' :: Maybe (Located Expr) -> Stmt
return' = ReturnS

var :: Named -> Expr
var = VarE

fun :: [Located Named] -> [Located Stmt] -> Expr
fun = FunE ()

obj :: [(Located Text, Located Expr)] -> Expr
obj = ObjE

bool :: Bool -> Expr
bool = BoolE

apply :: Located Expr -> [Located Expr] -> Expr
apply = ApplyE

assign :: Located Named -> Located Expr -> Expr
assign = AssignE

named :: Token -> Named
named (Name x) = Syntax.Named x ()

name :: Token -> Text
name (Name x) = x

infixl 4 <%>
(<%>) :: Functor f => (a -> b) -> f a -> f b
(<%>) = (<$>)

infixl 4 <%
(<%) :: Functor f => a -> f b -> f a
(<%) = (<$)
}