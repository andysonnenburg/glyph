{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.Lex
       ( lex
       ) where

import Control.Comonad
import Control.Monad.Error

import Data.ByteString.Lazy (ByteString)

import qualified Language.Glyph.Lex.Internal as Internal
import Language.Glyph.Location
import Language.Glyph.Parser
import Language.Glyph.Token

import Prelude ()

lex :: MonadError ParseException m => ByteString -> m [Located Token]
lex = runParserT m
  where
    m = do
      token <- Internal.lex
      case extract token of
        EOF ->
          return []
        _ -> do
          tokens <- m
          return (token : tokens)
