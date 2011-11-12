module Language.Glyph.Monoid
       ( module Data.Monoid
       , (<>)
       ) where

import Data.Monoid

infixl 9 <>
(<>) :: Monoid a => a -> a -> a
(<>) = mappend