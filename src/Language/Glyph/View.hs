{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Language.Glyph.View
       ( View (..)
       ) where

class View a b | a -> b where
  view :: a -> b
