{-# LANGUAGE NoImplicitPrelude #-}
module Language.Glyph.Monad.State.Class
       ( MonadState (..)
       ) where

import Language.Glyph.Monad

newtype StateT s m i j a = StateT { runStateT :: s -> m (a, s) }