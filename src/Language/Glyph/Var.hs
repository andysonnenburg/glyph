module Language.Glyph.Id
       ( Id (..)
       , Type (..)
       ) where

data Id = Id Name Type

data Type = Var | Fun