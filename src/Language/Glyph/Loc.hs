{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Language.Glyph.Loc
       ( Pos (..)
       , Loc (..)
       , seekPos
       ) where

import Data.Data
import Data.Semigroup

import Text.PrettyPrint.Free

data Pos = Pos Int Int deriving (Eq, Ord, Typeable, Data)

instance Show Pos where
  show = show . pretty

instance Pretty Pos where
  pretty (Pos r c) = pretty r <> char ':' <> pretty c

data Loc = Loc Pos Pos deriving (Typeable, Data)

instance Show Loc where
  show = show . pretty

instance Pretty Loc where
  pretty (Loc x y)
    | x == y =
        pretty x
    | Pos r1 c1 <- x, Pos r2 c2 <- y, r1 == r2 =
        pretty r1 <> char ':' <>
        pretty c1 <> char '-' <> pretty c2
    | otherwise =
        pretty x <> char '-' <> pretty y

instance Semigroup Loc where
  Loc a b <> Loc a' b' = Loc (min a a') (max b b')

seekPos :: Pos -> Char -> Pos
seekPos (Pos r c) x =
  case x of
    '\t' -> Pos r (((c + 7) `div` 8) * 8 + 1)
    '\n' -> Pos (r + 1) 1
    _ -> Pos r (c + 1)
