{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Language.Glyph.View
       ( View (..)
       ) where

class View a b | a -> b, b -> a where
  view :: a -> b
