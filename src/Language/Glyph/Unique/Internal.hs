module Language.Glyph.Unique.Internal
       ( Unique
       , intToUnique
       , uniqueToInt
       ) where

newtype Unique
  = Unique { unUnique :: Int
           } deriving (Eq, Ord, Show)

intToUnique :: Int -> Unique
intToUnique = Unique

uniqueToInt :: Unique -> Int
uniqueToInt = unUnique
