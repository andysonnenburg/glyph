{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.Parse
       ( ParseException (..)
       , parse
       ) where

import Control.Monad.Error

import Data.ByteString.Lazy (ByteString)

import Language.Glyph.Loc
import qualified Language.Glyph.Parse.Internal as Internal
import Language.Glyph.Parser
import Language.Glyph.Syntax
import Language.Glyph.Unique

parse :: ( MonadError ParseException m
        , UniqueMonad m
        ) => ByteString -> m [Stmt Loc]
parse = runParserT Internal.parse
