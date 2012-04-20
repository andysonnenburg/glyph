{-# LANGUAGE DeriveDataTypeable, Rank2Types #-}
module Language.Glyph.Generics
       ( module Data.Generics
       , NoData (..)
       , everythingButFuns
       , everythingThisScope
       , everywhereButM'
       , everywhereThisScope
       , everywhereThisScopeM
       , mkM'
       , extM'
       , ext1M'
       ) where

import Data.Generics

import Language.Glyph.Syntax

everythingButFuns :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingButFuns k f =
  everythingBut k (f' `ext1Q` gS `ext1Q` gE)
  where
    f' x = (f x, False)

    gS x@(FunDeclS {}) = (f x, True)
    gS x = (f x, False)

    gE x@(FunE {}) = (f x, True)
    gE x = (f x, False)

everythingThisScope :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingThisScope k f =
  everythingBut k (f' `ext1Q` gS `ext1Q` gE)
  where
    f' x = (f x, False)

    gS x@(FunDeclS {}) = (f x, True)
    gS x@(BlockS _) = (f x, True)
    gS x = (f x, False)

    gE x@(FunE {}) = (f x, True)
    gE x = (f x, False)

everywhereThisScope :: GenericT -> GenericT
everywhereThisScope =
  everywhereBut (const False `ext1Q` queryStmt `ext1Q` queryExpr)
  where
    queryStmt (FunDeclS {}) = True
    queryStmt (BlockS _) = True
    queryStmt _ = False

    queryExpr (FunE {}) = True
    queryExpr _ = False

everywhereThisScopeM :: Monad m => GenericM m -> GenericM m
everywhereThisScopeM f =
  everywhereButM' (f' `ext1M'` transformStmt `ext1M'` transformExpr)
  where
    f' x = do
      x' <- f x
      return (x', False)

    transformStmt x@(FunDeclS {}) = do
      x' <- f x
      return (x', True)
    transformStmt x@(BlockS _) = do
      x' <- f x
      return (x', True)
    transformStmt x = do
      x' <- f x
      return (x', False)

    transformExpr x@(FunE {}) = do
      x' <- f x
      return (x', True)
    transformExpr x = do
      x' <- f x
      return (x', False)

everywhereButM' :: Monad m => (forall a . Data a => a -> m (a, Bool)) -> GenericM m
everywhereButM' f x = do
  (x', stop) <- f x
  if stop
    then return x'
    else gmapM (everywhereButM' f) x'

mkM' :: (Monad m, Typeable a, Typeable b) => (b -> m (b, Bool)) -> a -> m (a, Bool)
mkM' = extM' f
  where
    f x = return (x, False)

extM' :: ( Monad m
         , Typeable a
         , Typeable b
         ) => (a -> m (a, Bool)) -> (b -> m (b, Bool)) -> a -> m (a, Bool)
extM' def ext = unM (M def `ext0` M ext)

ext1M' :: ( Monad m
         , Data d
         , Typeable1 t
         ) =>
         (forall e . Data e => e -> m (e, Bool)) ->
         (forall f . Data f => t f -> m (t f, Bool)) ->
         d -> m (d, Bool)
ext1M' def ext = unM (M def `ext1` M ext)

newtype M m x = M { unM :: x -> m (x, Bool) }

newtype NoData a = NoData { unNoData :: a } deriving Typeable

instance Typeable a => Data (NoData a) where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Language.Glyph.Generics.NoData"
