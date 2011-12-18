{
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, RecordWildCards #-}
{-# OPTIONS_GHC
    -fno-warn-lazy-unlifted-bindings
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-imports #-}
module Language.Glyph.HM.Lex.Internal
       ( lex
       ) where

import Control.Monad.Error

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Language.Glyph.Annotation.Location
import Language.Glyph.HM.Token hiding (True, False)
import qualified Language.Glyph.HM.Token as Token
import Language.Glyph.Lex.Alex
import Language.Glyph.Parser.Internal

import Prelude hiding (lex)
}

%wrapper "glyph"

$alpha = [a-zA-Z_]
$numeric = [0-9]

@name = $alpha [$alpha $numeric]*

:-

$white+ ;

<0> {
  \\ { backslash }
  "." { period }
  "=" { equals }
  "let" { let' }
  "in" { in' }
  "true" { true }
  "false" { false }
  @name { name }
}

{
special :: MonadError ParseException m => Token -> Action m
special x l _ _ = return (Annotated l x)

backslash :: MonadError ParseException m => Action m
backslash = special Backslash

period :: MonadError ParseException m => Action m
period = special Period

equals :: MonadError ParseException m => Action m
equals = special Equals

let' :: MonadError ParseException m => Action m
let' = special Let

in' :: MonadError ParseException m => Action m
in' = special In

true :: MonadError ParseException m => Action m
true = special Token.True

false :: MonadError ParseException m => Action m
false = special Token.False

name :: MonadError ParseException m => Action m
name l s n = do
  s' <- fromLazyByteString $ ByteString.take n' $ s
  return $ Annotated l $ Name $ s'
  where
    n' = fromIntegral n
}