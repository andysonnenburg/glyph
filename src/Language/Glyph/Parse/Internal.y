{
{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}
module Language.Glyph.Parse.Internal
       ( parse
       ) where

import Control.Comonad
import Control.Monad.Error hiding (void)
import Control.Monad.Identity hiding (void)
import Control.Monad.State hiding (void)

import Data.Functor.Apply
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)

import Language.Glyph.Annotation.Location
import Language.Glyph.Ident
import Language.Glyph.Lex.Internal
import Language.Glyph.Parser
import Language.Glyph.Syntax.Internal hiding (Expr, ExprView, Name, Stmt, StmtView)
import qualified Language.Glyph.Syntax.Internal as Syntax
import Language.Glyph.Token hiding (False, Return, True)
import qualified Language.Glyph.Token as Token
import Language.Glyph.Unique

import Prelude hiding (break, lex)
}

%tokentype { Located Token }

%token
VAR { (extract -> Var) }
FN { (extract -> Fn) }
NAME { (extract -> Name _) }
'.' { (extract -> Period) }
',' { (extract -> Comma) }
'(' { (extract -> LeftParenthesis) }
')' { (extract -> RightParenthesis) }
'{' { (extract -> LeftBrace) }
'}' { (extract -> RightBrace) }
';' { (extract -> Semicolon) }
':' { (extract -> Colon) }
TRUE { (extract -> Token.True) }
FALSE { (extract -> Token.False) }
VOID { (extract -> Token.Void) }
'!' { (extract -> Token.Bang) }
RETURN { (extract -> Token.Return) }
IF { (extract -> Token.If) }
ELSE { (extract -> Token.Else) }
WHILE { (extract -> Token.While) }
BREAK { (extract -> Token.Break) }
CONTINUE { (extract -> Token.Continue) }
THROW { (extract -> Token.Throw) }
TRY { (extract -> Token.Try) }
FINALLY { (extract -> Token.Finally) }
'=' { (extract -> Equals) }

%name stmts

%monad { (MonadError ParseException m, UniqueMonad m) } { ParserT m } { >>= } { return }

%lexer { lexer } { (extract -> EOF) }

%error { parseError }

%left ','
%right '='
%right '!'
%left '(' ')'

%%

top :: { [Stmt] }
  : manyStmts { $1 }

manyStmts :: { [Stmt] }
  : many(stmt) { $1 }

stmt :: { Stmt }
  : simple ';' { stmt $1 $2 (view $1) }
  | composite { $1 }

simple :: { Stmt }
  : expr { stmt $1 $1 (ExprS $1) }
  | varDecl { $1 }
  | return { $1 }
  | break { $1 }
  | continue { $1 }
  | throw { $1 }

composite :: { Stmt }
  : funDecl { $1 }
  | block { $1 }
  | ifThenElse { $1 }
  | while { $1 }
  | tryFinally { $1 }

expr :: { Expr }
  : var { $1 }
  | fun { $1 }
  | bool { $1 }
  | void { $1 }
  | not { $1 }
  | apply { $1 }
  | assign { $1 }
  | '(' expr ')' { $2 }

return :: { Stmt }
  : RETURN optionMaybe(expr) {
      stmt $1 (fromMaybe (location $1) (location <%> $2)) (return' $2)
    }

varDecl :: { Stmt }
  : VAR locatedName optionMaybe(snd('=', expr)) {
      stmt $1 (fromMaybe (location $2) (location <%> $3)) (varDecl (extract $2) $3)
    }

break :: { Stmt }
  : BREAK { stmt $1 $1 break }

continue :: { Stmt }
  : CONTINUE { stmt $1 $1 continue }

throw :: { Stmt }
  : THROW expr { stmt $1 $2 (throw $2) }

funDecl :: { Stmt }
  : FN name '(' parameters ')' '{' manyStmts '}' {
      stmt $1 $8 (funDecl $2 $4 $7)
    }

block :: { Stmt }
  : '{' manyStmts '}' { stmt $1 $3 (block $2) }

ifThenElse :: { Stmt }
  : IF '(' expr ')' block optionMaybe(snd(ELSE, block)) {
      stmt $1 (fromMaybe (location $5) (location <%> $6)) (ifThenElse $3 $5 $6)
    }

while :: { Stmt }
  : WHILE '(' expr ')' block {  stmt $1 $5 (while $3 $5) }

tryFinally :: { Stmt }
  : TRY block FINALLY block { stmt $1 $4 (tryFinally $2 $4) }

var :: { Expr }
  : locatedName { expr $1 $1 (var (extract $1)) }

fun :: { Expr }
  : FN '(' parameters ')' '{' manyStmts '}' {% liftM (expr $1 $7) (fun $3 $6) }

bool :: { Expr }
  : TRUE { expr $1 $1 (bool True) }
  | FALSE { expr $1 $1 (bool False) }

void :: { Expr }
  : VOID { expr $1 $1 void }

not :: { Expr }
  : '!' expr { expr $1 $2 (not' $2) }

apply :: { Expr }
  : expr '(' arguments ')' { expr $1 $4 (apply $1 $3) }

assign :: { Expr }
  : locatedName '=' expr { expr $1 $3 (assign (extract $1) $3) }

parameters :: { [Name] }
  : sepBy(name, ',') { $1 }

name :: { Name }
  : NAME {% newName' $1 }

locatedName :: { Located Name }
  : NAME {% liftM (<% $1) (newName' $1) }

arguments :: { [Expr] }
  : sepBy(expr, ',') { $1 }

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
type Stmt = Syntax.Stmt Location
type StmtView = Syntax.StmtView Location
type Expr = Syntax.Expr Location
type ExprView = Syntax.ExprView Location
type Name = Syntax.Name

parse :: ( MonadError ParseException m
         ,  UniqueMonad m
         ) => ParserT m [Stmt]
parse = stmts

lexer :: MonadError ParseException m => (Located Token -> ParserT m a) -> ParserT m a
lexer = (lex >>=)

parseError :: MonadError ParseException m =>
             Annotated Location Token ->
             ParserT m a
parseError x = throwError $ ParseError (location x) ("unexpected " ++ show x')
  where
    x' = extract x

varDecl :: Name -> Maybe Expr -> StmtView
varDecl = VarDeclS

funDecl :: Name -> [Name] -> [Stmt] -> StmtView
funDecl = FunDeclS

block :: [Stmt] -> StmtView
block = BlockS

return' :: Maybe Expr -> StmtView
return' = ReturnS

ifThenElse :: Expr -> Stmt -> Maybe (Stmt) -> StmtView
ifThenElse = IfThenElseS

while :: Expr -> Stmt -> StmtView
while = WhileS

tryFinally :: Stmt -> Stmt -> StmtView
tryFinally try finally = TryFinallyS try (Just finally)

break :: StmtView
break = BreakS

continue :: StmtView
continue = ContinueS

throw :: Expr -> StmtView
throw = ThrowS

var :: Name -> ExprView
var = VarE

fun :: UniqueMonad m => [Name] -> [Stmt] -> m ExprView
fun params stmts = do
  x <- freshIdent
  return $ FunE x params stmts

bool :: Bool -> ExprView
bool = LitE . BoolL

void :: ExprView
void = LitE VoidL

not' :: Expr -> ExprView
not' = NotE

apply :: Expr -> [Expr] -> ExprView
apply = ApplyE

assign :: Name -> Expr -> ExprView
assign = AssignE

name :: Token -> Text
name (Name x) = x

type Located = Annotated Location

stmt :: (HasLocation a, HasLocation b) => a -> b -> StmtView -> Stmt
stmt a b x = Syntax.Stmt (location a <> location b) x

expr :: (HasLocation a, HasLocation b) => a -> b -> ExprView -> Expr
expr a b x = Syntax.Expr (location a <> location b) x

newName :: UniqueMonad m => NameView -> m Name
newName x = liftM f freshIdent
  where
    f a = Syntax.Name a x

newName' :: UniqueMonad m => Located Token -> m Name
newName' = newName . name . extract

extract' :: Located a -> a
extract' = extract

infixl 4 <%>
(<%>) :: Functor f => (a -> b) -> f a -> f b
(<%>) = (<$>)

infixl 4 <%
(<%) :: Functor f => a -> f b -> f a
(<%) = (<$)
}
