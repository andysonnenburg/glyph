{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
module Data.WeakIntMap.Base
       ( WeakIntMap (..)
       , Key
         
       , find
         
       , empty
       , singleton
         
       , insert
         
       , adjust
       , adjustWithKey
       , updateWithKey
         
       , Tip (..)
       , Mask
       , Prefix
       , Nat
         
       , withNewTip
       , withTip
       , expunge
         
       , natFromInt
       , intFromNat
       , shiftRL
       , shiftLL
       , join
       , bin
       , zero
       , noMatch
       , mask
       , maskW
       , branchMask
       , highestBitMask
       ) where

import Control.Applicative hiding (empty)
import Control.Monad.Weak

import Data.Bits

import GHC.Exts (Word (..), Int (..), uncheckedShiftL#, uncheckedShiftRL#)

data WeakIntMap a
  = Bin
    {-# UNPACK #-} !Prefix
    {-# UNPACK #-} !Mask
    !(WeakIntMap a)
    !(WeakIntMap a)
  | Tip !(Weak (Tip a))
  | Nil

data Tip a = PairK {-# NOUNPACK #-} !Key a
type Prefix = Int
type Mask = Int
type Key = Int

find :: Key -> WeakIntMap a -> WeakM a
find = \ k -> withKey k . flip go
  where
    go k (Bin p m l r) | noMatch k p m = notFound k
                       | zero k m = go k l
                       | otherwise = go k r
    go k (Tip tip) = withTip tip $ \ m ->
      case m of
        Just (PairK kx x) | k == kx -> pure x
        _ -> error $ "WeakIntMap.find: key " ++ show k ++ " collected"
    go k Nil = notFound k
    notFound k = error ("WeakIntMap.find: key " ++ show k ++
                        " is not an element of the map")
{-# NOINLINE find #-}

withKey :: Key -> (Key -> WeakM a) -> WeakM a
withKey k f = k `seq` do
  a <- f k
  touchKey k
  return a

touchKey :: Key -> WeakM ()
touchKey = touch

empty :: WeakIntMap a
empty = Nil
{-# INLINE empty #-}

singleton :: Key -> a -> WeakM (WeakIntMap a)
singleton k x = withNewTip k x (\ _k tip -> pure $! Tip tip)
{-# INLINE singleton #-}

insert :: Key -> a -> WeakIntMap a -> WeakM (WeakIntMap a)
insert = \ k x t ->  withNewTip k x (\ k' tip -> go k' tip t)
  where
    go k tip t =
      case t of
        Bin p m l r
          | noMatch k p m -> join k (Tip tip) p <$> expunge t
          | zero k m -> Bin p m <$> go k tip l <*> expunge r
          | otherwise -> Bin p m <$> expunge l <*> go k tip r
        Tip tip' -> withTip tip' $ \ m ->
          pure $! case m of
            Just (PairK ky _)
              | k == ky -> Tip tip
              | otherwise -> join k (Tip tip) ky t
            Nothing -> Tip tip
        Nil -> pure $! Tip tip
{-# NOINLINE insert #-}

adjust ::  (a -> a) -> Key -> WeakIntMap a -> WeakM (WeakIntMap a)
adjust f = adjustWithKey (\ _ x -> f x)

adjustWithKey ::  (Key -> a -> a) -> Key -> WeakIntMap a -> WeakM (WeakIntMap a)
adjustWithKey f = updateWithKey (\ k' x -> Just (f k' x))

updateWithKey :: (Key -> a -> Maybe a) -> Key -> WeakIntMap a -> WeakM (WeakIntMap a)
updateWithKey f k t = k `seq`
  case t of
    Bin p m l r
      | noMatch k p m -> pure t
      | zero k m -> bin p m <$> updateWithKey f k l <*> pure r
      | otherwise -> bin p m l <$> updateWithKey f k r
    Tip w -> withTip w $ \ m ->
      case m of
        Just (PairK ky y)
          | k == ky ->
            case f k y of
              Just y' -> withNewTip ky y' (\ _ky tip -> pure $! Tip tip)
              Nothing -> pure Nil
          | otherwise -> pure t
        Nothing -> pure Nil
    Nil -> pure Nil

expunge :: WeakIntMap a -> WeakM (WeakIntMap a)
expunge t = fromExpunged t <$> expunge' t
{-# INLINE expunge #-}

expunge' :: WeakIntMap a -> WeakM (Expunged a)
expunge' t =
  case t of
    Bin p m l r -> do
      l' <- expunge' l
      r' <- expunge' r
      pure $! if wasChanged l' || wasChanged r'
              then Changed $ Bin p m (fromExpunged l l') (fromExpunged r r')
              else Unchanged
    Tip tip -> withTip tip $ \ m ->
      case m of
        Just _ -> pure Unchanged
        Nothing -> pure $ Changed Nil
    Nil -> pure Unchanged

wasChanged :: Expunged a -> Bool
wasChanged x =
  case x of
    Unchanged -> False
    Changed _ -> True

fromExpunged :: WeakIntMap a -> Expunged a -> WeakIntMap a
fromExpunged t x =
  case x of
    Unchanged -> t
    Changed t' -> t'

data Expunged a
  = Unchanged
  | Changed !(WeakIntMap a)

withNewTip :: Key -> a -> (Key -> Weak (Tip a) -> WeakM b) -> WeakM b
withNewTip k x f = k `seq` mkWeak k (PairK k x) >>= f k

withTip :: Weak (Tip a) -> (Maybe (Tip a) -> WeakM b) -> WeakM b
withTip tip f = deRefWeak tip >>= f

join :: Prefix -> WeakIntMap a -> Prefix -> WeakIntMap a -> WeakIntMap a
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
{-# INLINE join #-}

bin :: Prefix -> Mask -> WeakIntMap a -> WeakIntMap a -> WeakIntMap a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r = Bin p m l r
{-# INLINE bin #-}

zero :: Key -> Mask -> Bool
zero i m = natFromInt i .&. natFromInt m == 0
{-# INLINE zero #-}

noMatch :: Key -> Prefix -> Mask -> Bool
noMatch i p m = mask i m /= p
{-# INLINE noMatch #-}

mask :: Key -> Mask -> Prefix
mask i m = maskW (natFromInt i) (natFromInt m)
{-# INLINE mask #-}

maskW :: Nat -> Nat -> Prefix
maskW i m = intFromNat (i .&. (complement (m - 1) `xor` m))
{-# INLINE maskW #-}

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2 =
  intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))

highestBitMask :: Nat -> Nat
highestBitMask x0
  = case (x0 .|. shiftRL x0 1) of
     x1 -> case (x1 .|. shiftRL x1 2) of
      x2 -> case (x2 .|. shiftRL x2 4) of
       x3 -> case (x3 .|. shiftRL x3 8) of
        x4 -> case (x4 .|. shiftRL x4 16) of
#if WORD_SIZE_IN_BITS==32
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
#endif
          x6 -> (x6 `xor` (shiftRL x6 1))
{-# INLINE highestBitMask #-}

type Nat = Word

natFromInt :: Key -> Nat
natFromInt = fromIntegral
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Key
intFromNat = fromIntegral
{-# INLINE intFromNat #-}

shiftRL, shiftLL :: Nat -> Key -> Nat
shiftRL (W# x) (I# i) = W# (uncheckedShiftRL# x i)
shiftLL (W# x) (I# i) = W# (uncheckedShiftL#  x i)
{-# INLINE shiftRL #-}
{-# INLINE shiftLL #-}