{-# LANGUAGE ExistentialQuantification #-}
module Language.Glyph.Message
       ( Message (..)
       ) where

import Control.Exception

data Message
  = forall a. Show a => Warning a
  | forall e. Exception e => Error e
