{
{-# LANGUAGE
    FlexibleContexts
  , NoMonomorphismRestriction
  , PatternGuards
  , RecordWildCards #-}
{-# OPTIONS_GHC
    -fno-warn-lazy-unlifted-bindings
    -fno-warn-missing-signatures
    -fno-warn-unused-binds #-}
module Language.Glyph.Internal.Lex
       ( lex
       ) where

import Control.Monad.Error

import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as ByteString hiding (take, uncons)
import qualified Data.ByteString.Lazy as ByteString hiding (concat)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word

import Language.Glyph.Internal.Parser
import Language.Glyph.Internal.Location
import Language.Glyph.Token hiding (True, False)
import qualified Language.Glyph.Token as Token

import Prelude hiding (lex)
}

$alpha = [a-zA-Z_]
$numeric = [0-9]

@name = $alpha [$alpha $numeric]*

:-

$white+ ;

<0> {
  "var" { var }
  "fun" { fun }
  "return" { return' }
  "true" { true }
  "false" { false }
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
data AlexInput
  = AI { position :: Position
       , buffer :: ByteString
       }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AI p s)
  | Just (x, xs) <- ByteString.uncons s =
    let c = w2c x
        p' = seekPosition p c
    in Just (x, AI p' xs)
  | otherwise = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

type Action m = Location -> ByteString -> Int -> ParserT m (Located Token) 

special :: MonadError ParseException m => Token -> Action m
special x l _ _ = return (Located l x)

var :: MonadError ParseException m => Action m
var = special Var

fun :: MonadError ParseException m => Action m
fun = special Fun

name :: MonadError ParseException m => Action m
name l s n = do
  s' <- fromLazyByteString $ ByteString.take n' $ s
  return $ Located l $ Name $ s'
  where
    n' = fromIntegral n

fromLazyByteString :: MonadError ParseException m =>
                     ByteString ->
                     ParserT m Text
fromLazyByteString =
  either fromLeft fromRight .
  Text.decodeUtf8' .
  ByteString.concat .
  ByteString.toChunks
  where
    fromLeft e = do
      p <- getPosition
      let l = Location p p
      throwError $ UnicodeError l e
    fromRight =
      return

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

return' :: MonadError ParseException m => Action m
return' = special Return

equals :: MonadError ParseException m => Action m
equals = special Equals

lex :: MonadError ParseException m => ParserT m (Located Token)
lex = do
  i@(AI p s) <- getInput
  case alexScan i 0 of
    AlexEOF ->
      return $ Located (Location p p) EOF
    
    AlexError (AI p' _) ->
      throwError $ LexicalError (Location p p')
    
    AlexSkip i' _ -> do
      putInput i'
      lex
    
    AlexToken i'@(AI p' _) n m -> do
      putInput i'
      let l = Location p p'
      m l s n

getInput :: MonadError ParseException m => ParserT m AlexInput
getInput = do
  position <- getPosition
  buffer <- getBuffer
  return AI {..}

putInput :: MonadError ParseException m => AlexInput -> ParserT m ()
putInput AI {..} = do
  putPosition position
  putBuffer buffer
}