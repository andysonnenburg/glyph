{
{-# LANGUAGE
    FlexibleContexts
  , NoMonomorphismRestriction
  , RecordWildCards
  , TypeOperators #-}
{-# OPTIONS_GHC
    -fno-warn-lazy-unlifted-bindings
    -fno-warn-missing-signatures
    -fno-warn-unused-binds 
    -fno-warn-unused-imports #-}
module Language.Glyph.Lex.Internal
       ( lex
       ) where

import Control.Monad.Error hiding (void)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString hiding (foldl')
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Char (digitToInt)

import Language.Glyph.Loc
import Language.Glyph.Lex.Alex
import Language.Glyph.Parser.Internal
import Language.Glyph.Token hiding (True, False)
import qualified Language.Glyph.Token as Token

import Prelude hiding (break, lex)
}

%wrapper "glyph"

$alpha = [a-zA-Z_]
$numeric = [0-9]
$negative = \-

@int = $numeric+
@name = $alpha [$alpha $numeric]*

:-

$white+ ;

<0> {
  "var" { var }
  "fn" { fn }
  "return" { return' }
  "if" { if' }
  "else" { else' }
  "while" { while }
  "break" { break }
  "continue" { continue }
  "throw" { throw }
  "try" { try }
  "finally" { finally }
  "true" { true }
  "false" { false }
  "void" { void }
  "{" { leftBrace }
  "}" { rightBrace }
  ":" { colon }
  ";" { semicolon }
  "(" { leftParenthesis }
  ")" { rightParenthesis }
  "." { period }
  "!" { bang }
  "+" { plus }
  "-" { minus }
  "==" { equals }
  "=" { assign }
  "," { comma }
  @int { int }
  $negative @int { negativeInt }
  @name { name }
}

{  
special :: MonadError ParseException m => TokenView -> Action m
special x l _ _ = return $ Token l x

var :: MonadError ParseException m => Action m
var = special Var

fn :: MonadError ParseException m => Action m
fn = special Fn

int :: MonadError ParseException m => Action m
int l s n =
  return .
  Token l .
  Int .
  fromIntegral .
  int' $
  take' n s

negativeInt :: MonadError ParseException m => Action m
negativeInt l s n = 
  return .
  Token l .
  Int .
  fromIntegral .
  negate .
  int' .
  ByteString.tail $
  take' n s

int' :: ByteString -> Integer
int' = ByteString.foldl' f 0
  where
    f a b = a * 10 + toInteger (digitToInt b)

name :: MonadError ParseException m => Action m
name l s n = do
  s' <- fromLazyByteString $ take' n s
  return $ Token l (Name s')

take' :: Int -> ByteString -> ByteString
take' n = ByteString.take (fromIntegral n)

leftBrace :: MonadError ParseException m => Action m
leftBrace = special LeftBrace

rightBrace :: MonadError ParseException m => Action m
rightBrace = special RightBrace

colon :: MonadError ParseException m => Action m
colon = special Colon

semicolon :: MonadError ParseException m => Action m
semicolon = special Semicolon

true :: MonadError ParseException m => Action m
true = special Token.True

false :: MonadError ParseException m => Action m
false = special Token.False

void :: MonadError ParseException m => Action m
void = special Token.Void

return' :: MonadError ParseException m => Action m
return' = special Return

if' :: MonadError ParseException m => Action m
if' = special If

else' :: MonadError ParseException m => Action m
else' = special Else

while :: MonadError ParseException m => Action m
while = special While

break :: MonadError ParseException m => Action m
break = special Break

continue :: MonadError ParseException m => Action m
continue = special Continue

throw :: MonadError ParseException m => Action m
throw = special Throw

try :: MonadError ParseException m => Action m
try = special Try

finally :: MonadError ParseException m => Action m
finally = special Finally

leftParenthesis :: MonadError ParseException m => Action m
leftParenthesis = special LeftParenthesis

rightParenthesis :: MonadError ParseException m => Action m
rightParenthesis = special RightParenthesis

period :: MonadError ParseException m => Action m
period = special Period

bang :: MonadError ParseException m => Action m
bang = special Bang

plus :: MonadError ParseException m => Action m
plus = special Plus

minus :: MonadError ParseException m => Action m 
minus = special Minus

equals :: MonadError ParseException m => Action m
equals = special Equals

assign :: MonadError ParseException m => Action m 
assign = special Assign

comma :: MonadError ParseException m => Action m
comma = special Comma
}
