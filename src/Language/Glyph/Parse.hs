{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.Parse
       ( ParseException (..)
       , parse
       ) where

import Control.Monad.Error

import Data.ByteString.Lazy (ByteString)

import Language.Glyph.Location
import qualified Language.Glyph.Internal.Parse as Internal
import Language.Glyph.Parser
import Language.Glyph.Syntax

parse :: MonadError ParseException m =>
        ByteString ->
        m [Located (Stmt ())]
parse = runParserT Internal.parse