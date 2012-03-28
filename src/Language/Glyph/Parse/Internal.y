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

import Language.Glyph.Ident
import Language.Glyph.Lex.Internal
import Language.Glyph.Loc
import Language.Glyph.Parser
import Language.Glyph.Syntax hiding (Expr, ExprView, Name, Stmt, StmtView)
import qualified Language.Glyph.Syntax as Syntax
import Language.Glyph.Token hiding (False, Return, True, loc)
import qualified Language.Glyph.Token as Token
import Language.Glyph.Unique

import Prelude hiding (break, lex)
}

%tokentype { Token }

%token
VAR { (view -> Var) }
FN { (view -> Fn) }
NAME { (view -> Name _) }
'.' { (view -> Period) }
',' { (view -> Comma) }
'(' { (view -> LeftParenthesis) }
')' { (view -> RightParenthesis) }
'{' { (view -> LeftBrace) }
'}' { (view -> RightBrace) }
';' { (view -> Semicolon) }
':' { (view -> Colon) }
INT { (view -> Int _) }
TRUE { (view -> Token.True) }
FALSE { (view -> Token.False) }
VOID { (view -> Token.Void) }
'!' { (view -> Token.Bang) }
RETURN { (view -> Token.Return) }
IF { (view -> Token.If) }
ELSE { (view -> Token.Else) }
WHILE { (view -> Token.While) }
BREAK { (view -> Token.Break) }
CONTINUE { (view -> Token.Continue) }
THROW { (view -> Token.Throw) }
TRY { (view -> Token.Try) }
FINALLY { (view -> Token.Finally) }
'=' { (view -> Equals) }

%name stmts

%monad { (MonadError ParseException m, UniqueMonad m) } { ParserT m } { >>= } { return }

%lexer { lexer } { (view -> EOF) }

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
      stmt $1 (fromMaybe (loc $1) (loc <%> $2)) (return' $2)
    }

varDecl :: { Stmt }
  : VAR nameWithLoc optionMaybe(snd('=', expr)) {
      stmt $1 (fromMaybe (loc $2) (loc <%> $3)) (varDecl (extract $2) $3)
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
      stmt $1 (fromMaybe (loc $5) (loc <%> $6)) (ifThenElse $3 $5 $6)
    }

while :: { Stmt }
  : WHILE '(' expr ')' block {  stmt $1 $5 (while $3 $5) }

tryFinally :: { Stmt }
  : TRY block FINALLY block { stmt $1 $4 (tryFinally $2 $4) }

var :: { Expr }
  : nameWithLoc { expr $1 $1 (var (extract $1)) }

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
  : nameWithLoc '=' expr { expr $1 $3 (assign (extract $1) $3) }

parameters :: { [Name] }
  : sepBy(name, ',') { $1 }

name :: { Name }
  : NAME {% newName' $1 }

nameWithLoc :: { WithLoc Name }
  : NAME {% newNameWithLoc $1 }

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
type Stmt = Syntax.Stmt Loc
type StmtView = Syntax.StmtView Loc
type Expr = Syntax.Expr Loc
type ExprView = Syntax.ExprView Loc
type Name = Syntax.Name

parse :: ( MonadError ParseException m
         ,  UniqueMonad m
         ) => ParserT m [Stmt]
parse = stmts

lexer :: MonadError ParseException m => (Token -> ParserT m a) -> ParserT m a
lexer = (lex >>=)

parseError :: MonadError ParseException m =>
             Token ->
             ParserT m a
parseError (Token a x) = throwError $ ParseError a ("unexpected " ++ show x)

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

name :: Token -> NameView
name (view -> Name x) = x

stmt :: (HasLoc a, HasLoc b) => a -> b -> StmtView -> Stmt
stmt a b x = Syntax.Stmt (loc a <> loc b) x

expr :: (HasLoc a, HasLoc b) => a -> b -> ExprView -> Expr
expr a b x = Syntax.Expr (loc a <> loc b) x

newName :: UniqueMonad m => NameView -> m Name
newName x = liftM f freshIdent
  where
    f a = Syntax.Name a x

newName' :: UniqueMonad m => Token -> m Name
newName' = newName . name

newNameWithLoc :: UniqueMonad m => Token -> m (WithLoc Name)
newNameWithLoc x = do
  x' <- newName' x
  return $ WithLoc x' $ loc x

data WithLoc a = WithLoc a Loc

instance Functor WithLoc where
  fmap f (WithLoc a l) = WithLoc (f a) l

instance Extend WithLoc where
  duplicate x@(WithLoc _ l) = WithLoc x l
  extend f x@(WithLoc _ l) = WithLoc (f x) l

instance Comonad WithLoc where
  extract (WithLoc a _) = a

class HasLoc a where
  loc :: a -> Loc

instance HasLoc Loc where
  loc = id

instance HasLoc Token where
  loc = Token.loc

instance HasLoc Stmt where
  loc (Syntax.Stmt x _) = x

instance HasLoc Expr where
  loc (Syntax.Expr x _) = x

instance HasLoc (WithLoc a) where
  loc (WithLoc _ x) = x

infixl 4 <%>
(<%>) :: Functor f => (a -> b) -> f a -> f b
(<%>) = (<$>)

infixl 4 <%
(<%) :: Functor f => b -> f a -> f b
(<%) = (<$)
}
