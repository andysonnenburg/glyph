{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.Parse
       ( ParseException (..)
       , parse
       ) where

import Control.Monad.Error

import Data.ByteString.Lazy (ByteString)

import Language.Glyph.Annotation.Location
import Language.Glyph.Ident
import qualified Language.Glyph.Parse.Internal as Internal
import Language.Glyph.Parser
import Language.Glyph.Syntax

parse :: ( MonadError ParseException m
        , MonadIdentSupply m
        ) => ByteString -> m [Stmt Location]
parse = runParserT Internal.parse