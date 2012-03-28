{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Language.Glyph.Lex
       ( lex
       ) where

import Control.Monad.Error

import Data.ByteString.Lazy (ByteString)

import qualified Language.Glyph.Lex.Internal as Internal
import Language.Glyph.Parser
import Language.Glyph.Token
import Language.Glyph.View

import Prelude ()

lex :: MonadError ParseException m => ByteString -> m [Token]
lex = runParserT m
  where
    m = do
      x <- Internal.lex
      case view x of
        EOF ->
          return []
        _ -> do
          xs <- m
          return (x : xs)
