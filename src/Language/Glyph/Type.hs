{-# LANGUAGE DeriveDataTypeable #-}
module Language.Glyph.Type
       ( TypeScheme (..)
       , Type (..)
       , Label
       , Var
       ) where

import Data.Data
import Data.List
import Data.Map (Map)

import Language.Glyph.Ident.Internal

data TypeScheme = Forall [Var] Type deriving (Show, Typeable, Data)

data Type
  = Var Var
  | Record (Map Label Type)
  | Type :->: Type
  | Int
  | Double
  | Bool
  | Void
    
  | Tuple [Type]
  | Cont Type deriving (Typeable, Data)

infixr 9 :->:

instance Show Type where
  show x =
    case x of
      Var a -> "#" ++ (show . unIdent) a
      a :->: b -> show a ++ " -> " ++ show b
      Int -> "int"
      Double -> "double"
      Bool -> "bool"
      Void -> "void"
      Tuple xs -> "(" ++ intercalate ", " (map show xs) ++ ")"
      Cont a -> "#Cont " ++ show a

type Label = String

type Var = Ident