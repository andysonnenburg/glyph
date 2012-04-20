{-# LANGUAGE MagicHash #-}
module Language.Glyph.List.Strict
       ( List (..)
       , (!!)
       , foldM
       , fromList
       , length
       , replicate
       , replicateM
       , singleton
       , zip
       ) where

import Control.Applicative
import Control.DeepSeq

import Data.Foldable
import Data.Hashable
import Data.Monoid
import Data.Traversable

import GHC.Exts

import Prelude hiding ((!!), foldl, foldr, foldr1, length, replicate, sequence, zip)

data List a
  = Nil
  | !a :| !(List a) deriving (Eq, Show)

infixr 5 :|

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (x :| xs) ys = x :| xs `mappend` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (x :| xs) = f x :| fmap f xs

instance Foldable List where
  {-# INLINE [0] foldr #-}
  foldr k z = go
    where
      go Nil = z
      go (y :| ys) = y `k` go ys

  foldl f z0 xs0 = go z0 xs0
    where
      go z Nil = z
      go z (x :| xs) = go (f z x) xs

  foldr1 _ (x :| Nil) = x
  foldr1 f (x :| xs) = f x (foldr1 f xs)
  foldr1 _ Nil = error "Language.Glyph.List.Strict.foldr1: empty list"

  foldl1 f (x :| xs) = foldl f x xs
  foldl1 _ Nil = error "Language.Glyph.List.Strict.foldl1: empty list"

instance Traversable List where
  {-# INLINE traverse #-}
  traverse f = foldr f' (pure Nil)
    where
      f' x ys = (:|) <$> f x <*> ys
  
  {-# INLINE mapM #-}
  mapM f as = sequence (fmap f as)
  
  {-# INLINE sequence #-}
  sequence ms = foldr k (return Nil) ms
    where
      k m m' = do { x <- m; xs <- m'; return (x :| xs) }

instance Hashable a => Hashable (List a) where
  hashWithSalt = foldl' hashWithSalt

instance NFData a => NFData (List a) where
  rnf Nil = ()
  rnf (x :| xs) = rnf x `seq` rnf xs

(!!) :: List a -> Int -> a
xs !! (I# n0)
  | n0 <# 0# = error "Language.Glyph.List.Strict.(!!): negative index"
  | otherwise = sub xs n0
  where
    sub :: List a -> Int# -> a
    sub Nil _ = error "Language.Glyph.List.Strict.(!!): index too large"
    sub (y :| ys) n = if n ==# 0#
                      then y
                      else sub ys (n -# 1#)

foldM :: Monad m => (a -> b -> m a) -> a -> List b -> m a
foldM _ a Nil = return a
foldM f a (x :| xs) = f a x >>= \ a' -> foldM f a' xs

fromList :: [a] -> List a
fromList = foldr' (:|) Nil

length :: List a -> Int
length l = len l 0#
  where
    len :: List a -> Int# -> Int
    len Nil a# = I# a#
    len (_ :| xs) a# = len xs (a# +# 1#)

replicate :: Int -> a -> List a
{-# INLINE replicate #-}
replicate n x = go n
  where
    go n'
      | n' <= 0 = Nil
      | otherwise  = x :| go (n' - 1)

replicateM :: Monad m => Int -> m a -> m (List a)
replicateM n x = sequence (replicate n x)

singleton :: a -> List a
singleton x = x :| Nil

zip :: List a -> List b -> List (a, b)
zip (a :| as) (b :| bs) = (a, b) :| zip as bs
zip _ _ = Nil