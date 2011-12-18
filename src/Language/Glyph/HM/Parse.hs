{-# LANGUAGE FlexibleContexts #-}
module Language.Glyph.HM.Parse
       ( ParseException (..)
       , parse
       ) where

import Control.Monad.Error

import Data.ByteString.Lazy (ByteString)

import Language.Glyph.Annotation.Location
import qualified Language.Glyph.HM.Parse.Internal as Internal
import Language.Glyph.HM.Syntax
import Language.Glyph.Parser

parse :: MonadError ParseException m =>
        ByteString ->
        m (Annotated Location (Expr Location ()))
parse = runParserT Internal.parse