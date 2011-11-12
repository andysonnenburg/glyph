{-# LANGUAGE NoImplicitPrelude #-}
module Language.Glyph.Monad.Symtab.Class
       ( MonadSymtab (..)
       ) where

import Language.Glyph.Location
import Language.Glyph.Monad
import Language.Glyph.Name
import Language.Glyph.NameMap (NameMap)
import Language.Glyph.Syntax

class Monad m => MonadSymtab m where
  getStmts :: m i i [Located (Stmt Name)]
  getSymtab :: m i i (NameMap i)
  putSymtab :: NameMap j -> m i j ()