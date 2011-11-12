{-# LANGUAGE DeriveDataTypeable #-}
module Language.Glyph.Internal.Type
       ( Constraint (..)
       , TypeScheme (..)
       , Type (..)
       , Label
       , Var (..)
       ) where

import Data.Data
import Data.Map (Map)

import Prelude hiding (True)

data Constraint
  = True
  | Type :==: Type
  | Constraint :&&: Constraint
  | Type :.: (String, Type) deriving (Show, Typeable, Data)

infixl 8 :==:

data TypeScheme
  = Type Type
  | Forall Var Constraint TypeScheme deriving (Show, Typeable, Data)

data Type
  = Var Var
  | Record (Map Label Type)
  | [Type] :->: Type
  | Int
  | Double
  | Bool
  | Void deriving (Show, Typeable, Data)

type Label = String

type Var = Int