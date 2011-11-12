{-# LANGUAGE DeriveDataTypeable #-}
module Language.Glyph.Annotation.With
       ( With (..)
       ) where

import Data.Data

data With a b = With a b deriving (Show, Typeable, Data)
