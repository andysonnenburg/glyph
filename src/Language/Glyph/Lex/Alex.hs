{-# LANGUAGE FlexibleContexts, PatternGuards, RecordWildCards #-}
module Language.Glyph.Lex.Alex
       ( AlexInput
       , alexGetByte
       , alexInputPrevChar
       , fromLazyByteString
       , getInput
       , putInput
       ) where

import Control.Monad.Error

import qualified Data.ByteString as ByteString hiding (take, uncons)
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString hiding (concat)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word

import Language.Glyph.Annotation.Location
import Language.Glyph.Parser.Internal

type AlexInput = S

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (S {..})
  | Just (x, xs) <- ByteString.uncons buffer =
    let position' = seekPosition position (w2c x)
    in Just (x, S { position = position', buffer = xs })
  | otherwise = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

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
      S {..} <- get
      throwError $ UnicodeError (Location position position) e
    fromRight =
      return

getInput :: MonadError ParseException m => ParserT m AlexInput
getInput = get

putInput :: MonadError ParseException m => AlexInput -> ParserT m ()
putInput = put
