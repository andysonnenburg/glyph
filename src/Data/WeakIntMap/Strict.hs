module Data.WeakIntMap.Strict
       ( WeakIntMap
       , Key
         
       , find
         
       , empty
       , singleton
         
       , insert
         
       , adjust
       , adjustWithKey
       , updateWithKey
       ) where

import Control.Applicative hiding (empty)
import Control.Monad.Weak

import Data.WeakIntMap.Base hiding (singleton,
                                    insert,
                                    adjust,
                                    adjustWithKey,
                                    updateWithKey)

singleton :: Key -> a -> WeakM (WeakIntMap a)
singleton k x = x `seq` withNewTip k x (\ _k tip -> pure $! Tip tip)
{-# INLINE singleton #-}

insert :: Key -> a -> WeakIntMap a -> WeakM (WeakIntMap a)
insert = \ k x t -> k `seq` x `seq` withNewTip k x (\ k' tip -> go k' tip t)
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
              Just y' -> y' `seq` withNewTip ky y' (\ _ky tip -> pure $! Tip tip)
              Nothing -> pure Nil
          | otherwise -> pure t
        Nothing -> pure Nil
    Nil -> pure Nil