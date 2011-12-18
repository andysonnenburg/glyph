{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleInstances
  , MultiParamTypeClasses
  , StandaloneDeriving #-}
module Language.Glyph.Annotation
       ( module Language.Glyph.View
       , Annotated (..)
       ) where

import Control.Comonad

import Data.Data
import Data.Foldable
import Data.Functor.Apply
import Data.Semigroup
import Data.Traversable

import Language.Glyph.View

data Annotated a b = Annotated a b deriving (Show, Typeable, Data)

deriving instance Foldable (Annotated a)
deriving instance Traversable (Annotated a)
deriving instance Functor (Annotated a)

instance Semigroup a => Apply (Annotated a) where
  Annotated x f <.> Annotated y a = Annotated (x <> y) (f a)

instance Extend (Annotated a) where
  duplicate w@(Annotated a _) = Annotated a w
  extend f w@(Annotated a _) = Annotated a (f w)

instance Comonad (Annotated a) where
  extract (Annotated _ a) = a

instance View (Annotated a b) a where
  view (Annotated x _) = x
  Annotated _ a `updateView` x = Annotated x a