{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Symtab.Instances () where

import Control.Monad.Reader

import Language.Glyph.Symtab.Class

instance MonadSymtab r' m => MonadSymtab r' (ReaderT r m) where
  askSymtab = lift askSymtab