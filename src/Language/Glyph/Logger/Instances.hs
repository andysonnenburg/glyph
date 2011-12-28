{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Logger.Instances () where

import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict

import Language.Glyph.Logger.Class

import Prelude hiding (log)

instance MonadLogger w m => MonadLogger w (ReaderT r m) where
  log = lift . log

instance MonadLogger w m => MonadLogger w (Lazy.StateT s m) where
  log = lift . log

instance MonadLogger w m => MonadLogger w (Strict.StateT s m) where
  log = lift . log