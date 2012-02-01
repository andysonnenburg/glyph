{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.Parse
       ( ParseException (..)
       , parse
       ) where

import Control.Monad.Error

import Data.ByteString.Lazy (ByteString)

import Language.Glyph.Annotation.Location
import qualified Language.Glyph.Parse.Internal as Internal
import Language.Glyph.Parser
import Language.Glyph.Syntax
import Language.Glyph.Unique

parse :: ( MonadError ParseException m
        , UniqueMonad m
        ) => ByteString -> m [Stmt Location]
parse = runParserT Internal.parse
