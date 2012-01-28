{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses #-}
module Language.Glyph.Update
       ( Update (..)
       ) where

class Update a b | a -> b where
  update :: a -> b -> a
