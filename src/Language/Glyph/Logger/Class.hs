{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Language.Glyph.Logger.Class
       ( MonadLogger (..)
       ) where

class Monad m => MonadLogger w m | m -> w where
  log :: w -> m ()
