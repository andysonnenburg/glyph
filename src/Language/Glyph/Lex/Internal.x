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
import qualified Data.ByteString.Lazy as ByteString

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
  "!" { bang }
  "." { period }
  "," { comma }
  "(" { leftParenthesis }
  ")" { rightParenthesis }
  "{" { leftBrace }
  "}" { rightBrace }
  ":" { colon }
  ";" { semicolon }
  "=" { equals }
  @name { name }
}

{  
special :: MonadError ParseException m => TokenView -> Action m
special x l _ _ = return $ Token l x

var :: MonadError ParseException m => Action m
var = special Var

fn :: MonadError ParseException m => Action m
fn = special Fn

name :: MonadError ParseException m => Action m
name l s n = do
  s' <- fromLazyByteString $ ByteString.take n' $ s
  return $ Token l (Name s')
  where
    n' = fromIntegral n

period :: MonadError ParseException m => Action m
period = special Period

comma :: MonadError ParseException m => Action m
comma = special Comma

leftParenthesis :: MonadError ParseException m => Action m
leftParenthesis = special LeftParenthesis

rightParenthesis :: MonadError ParseException m => Action m
rightParenthesis = special RightParenthesis

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

bang :: MonadError ParseException m => Action m
bang = special Bang

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

equals :: MonadError ParseException m => Action m
equals = special Equals
}
