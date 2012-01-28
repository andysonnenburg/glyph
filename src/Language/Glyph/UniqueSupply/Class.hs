module Language.Glyph.UniqueSupply.Class
       ( Unique
       , MonadUniqueSupply (..)
       ) where

import Language.Glyph.Unique

class Monad m => MonadUniqueSupply m where
  freshUnique :: m Unique