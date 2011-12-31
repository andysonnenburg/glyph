{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Language.Glyph.Annotation.Location.Class
       ( Position (..)
       , Location (..)
       , HasLocation (..)
       , seekPosition
       ) where

import Data.Data
import Data.Semigroup

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

instance Semigroup Location where
  Location begin end <> Location begin' end' =
    Location (min begin begin') (max end end')

instance Monoid Location where
  mempty = Location (Position 1 1) (Position 1 1)
  mappend = (<>)

class HasLocation a where
  location :: a -> Location

seekPosition :: Position -> Char -> Position
seekPosition (Position r c) x =
  case x of
    '\t' -> Position r (((c + 7) `div` 8) * 8 + 1)
    '\n' -> Position (r + 1) 1
    _ -> Position r (c + 1)
