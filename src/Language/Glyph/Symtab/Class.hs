{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Language.Glyph.Symtab.Class
       ( MonadSymtab (..)
       ) where

import Language.Glyph.IdentMap

class Monad m => MonadSymtab r m | m -> r where
  askSymtab :: m (IdentMap r)