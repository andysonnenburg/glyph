{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , PatternGuards #-}
module Language.Glyph.Internal.Location
       ( Position (..)
       , Location (..)
       , Located (..)
       , location
       , seekPosition
       ) where

import Control.Comonad

import Data.Data
import Data.Foldable
import Data.Functor.Apply
import Data.Semigroup
import Data.Traversable

data Position
  = Position Int Int
  deriving (Eq, Ord, Typeable, Data)

instance Show Position where
  show (Position r c) = show r ++ ":" ++ show c

data Location
  = Location Position Position
  deriving (Typeable, Data)

instance Show Location where
  show (Location x y)
    | x == y =
        show x
    | Position r1 c1 <- x, Position r2 c2 <- y, r1 == r2 =
        show r1 ++ ":" ++
        show c1 ++ "-" ++ show c2
    | otherwise =
        show x ++ "-" ++ show y

data Located a
  = Located Location a
  deriving ( Show
           , Functor
           , Foldable
           , Traversable
           , Typeable
           , Data
           )

seekPosition :: Position -> Char -> Position
seekPosition (Position r c) x =
  case x of
    '\t' -> Position r (((c + 7) `div` 8) * 8 + 1)
    '\n' -> Position (r + 1) 1
    _ -> Position r (c + 1)

location :: Located a -> Location
location (Located x _) = x

instance Semigroup Location where
  Location a b <> Location c d = Location (min a c) (max b d)

instance Extend Located where
  duplicate w@(Located x _) = Located x w
  extend f w@(Located x _) = Located x (f w)

instance Comonad Located where
  extract (Located _ a) = a

instance Apply Located where
  Located x f <.> Located y a = Located (x <> y) (f a)
