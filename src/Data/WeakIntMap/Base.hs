{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif
{-# OPTIONS_GHC -fglasgow-exts #-}
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
         
       , touchKey
         
       , Mask
       , Prefix
       , Nat
         
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

import Data.Bits
import Data.Maybe

#if __GLASGOW_HASKELL__
import GHC.Base (IO (..))
import GHC.Exts (Word (..), Int (..))
import GHC.Prim (touch#, uncheckedShiftL#, uncheckedShiftRL#)
#else
import Data.Word
#endif

import System.Mem.Weak

import System.IO
-- import Debug.Trace
trace x y = y

data WeakIntMap a
  = Bin
    {-# UNPACK #-} !Prefix
    {-# UNPACK #-} !Mask
    !(WeakIntMap a)
    !(WeakIntMap a)
  | Tip !(Weak (Tip a))
  | Nil

data Tip a = PairK {-# UNPACK #-} !Key a
type Prefix = Int
type Mask = Int
type Key = Int

find :: Key -> WeakIntMap a -> IO a
find k t = k `seq` do
  x <- go t
  touchKey k
  return x
  where
    go (Bin p m l r) | noMatch k p m = notFound
                     | zero k m = go l
                     | otherwise = go r
    go (Tip tip) = withTip tip $ \ m ->
      case m of
        Just (PairK kx x) | k == kx -> pure x
        _ -> error $ "find: " ++ show k ++ " collected"
    go Nil = notFound
    notFound = error ("WeakIntMap.find: key " ++ show k ++
                      " is not an element of the map")

empty :: WeakIntMap a
empty = Nil
{-# INLINE empty #-}

singleton :: Key -> a -> IO (WeakIntMap a)
singleton k x = Tip <$> mkTip k x
{-# INLINE singleton #-}

insert :: Key -> a -> WeakIntMap a -> IO (WeakIntMap a)
insert k x t = k `seq` do
  t' <- go
  touchKey k
  return t'
  where
    go = case t of
      Bin p m l r
        | noMatch k p m -> join k <$> (Tip <$> mkTip k x) <*> pure p <*> expunge t
        | zero k m -> Bin p m <$> insert k x l <*> expunge r
        | otherwise -> Bin p m <$> expunge l <*> insert k x r
      Tip tip -> withTip tip $ \ m ->
        case m of
          Just (PairK ky _)
            | k == ky -> Tip <$> mkTip k x
            | otherwise -> join k <$> (Tip <$> mkTip k x) <*> pure ky <*> pure t
          _ -> ("insert: " ++ show k ++ " collected") `trace` (Tip <$> mkTip k x)
      Nil -> Tip <$> mkTip k x

adjust ::  (a -> a) -> Key -> WeakIntMap a -> IO (WeakIntMap a)
adjust f k m = adjustWithKey (\ _ x -> f x) k m

adjustWithKey ::  (Key -> a -> a) -> Key -> WeakIntMap a -> IO (WeakIntMap a)
adjustWithKey f = updateWithKey (\ k' x -> Just (f k' x))

updateWithKey :: (Key -> a -> Maybe a) -> Key -> WeakIntMap a -> IO (WeakIntMap a)
updateWithKey f k t = k `seq` do
  t' <- go
  touchKey k
  return t'
  where
    go = case t of
      Bin p m l r
        | noMatch k p m -> pure t
        | zero k m -> bin p m <$> updateWithKey f k l <*> expunge r
        | otherwise -> bin p m <$> expunge l <*> updateWithKey f k r
      Tip tip -> withTip tip $ \ m ->
        case m of
          Just (PairK ky y)
            | k == ky ->
              case f k y of
                Just y' -> Tip <$> mkTip ky y'
                Nothing -> pure Nil
            | otherwise -> pure t
          Nothing -> ("updateWithKey: " ++ show k ++ " collected") `trace` pure Nil
      Nil -> pure Nil

expunge :: WeakIntMap a -> IO (WeakIntMap a)
expunge t = fromMaybe t <$> expunge' t
{-# INLINE expunge #-}

expunge' :: WeakIntMap a -> IO (Maybe (WeakIntMap a))
expunge' t =
  case t of
    Bin p m l r -> do
      l' <- expunge' l
      r' <- expunge' r
      pure $! if isJust l' || isJust r'
              then Just $! Bin p m (fromMaybe l l') (fromMaybe r r')
              else Nothing
    Tip tip -> withTip tip $ \ m ->
      case m of
        Just _ -> pure Nothing
        Nothing -> pure $ Just Nil
    Nil -> pure Nothing

mkTip :: Key -> a -> IO (Weak (Tip a))
mkTip k x = k `seq` do
  tip <- mkWeak k (PairK k x) (Just (hPutStrLn stderr $ "mkTip: " ++ show k ++ " collected"))
  touchKey k
  return tip

withTip :: Weak (Tip a) -> (Maybe (Tip a) -> IO b) -> IO b
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
#if !(defined(__GLASGOW_HASKELL__) && WORD_SIZE_IN_BITS==32)
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
#endif
          x6 -> (x6 `xor` (shiftRL x6 1))
{-# INLINE highestBitMask #-}

touchKey :: Key -> IO ()
#if __GLASGOW_HASKELL__
touchKey k = IO $ \ s -> case touch# k s of s' -> (# s', () #)
#else
touchKey _ = pure ()
#endif

type Nat = Word

natFromInt :: Key -> Nat
natFromInt = fromIntegral
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Key
intFromNat = fromIntegral
{-# INLINE intFromNat #-}

shiftRL, shiftLL :: Nat -> Key -> Nat
#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
shiftRL (W# x) (I# i) = W# (uncheckedShiftRL# x i)
shiftLL (W# x) (I# i) = W# (uncheckedShiftL#  x i)
#else
shiftRL x i   = shiftR x i
shiftLL x i   = shiftL x i
#endif
{-# INLINE shiftRL #-}
{-# INLINE shiftLL #-}