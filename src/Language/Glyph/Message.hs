{-# LANGUAGE ExistentialQuantification #-}
module Language.Glyph.Message
       ( Message (..)
       ) where

import Control.Exception

import Data.Monoid

data Message
  = forall a. Show a => Warning a
  | forall e. Exception e => Error e

instance Monoid Message