{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, MultiParamTypeClasses #-}
module Language.Glyph.HM.Syntax
       ( Exp (..)
       , ExpView (..)
       , Pat (..)
       , varE
       , appE
       , absE
       , letE
       , boolE
       , voidE
       , intE
       , doubleE
       , tupleE
       , undefined'
       , asTypeOf'
       , fix'
       , runCont
       , callCC
       , return'
       , then'
       , varP
       , tupleP
       ) where

import Control.Monad
import Control.Monad.Reader

import Data.Data

import Language.Glyph.Ident
import Language.Glyph.View

data Exp a = Exp a (ExpView a) deriving (Show, Typeable, Data, Functor)

data ExpView a
  = VarE Ident
  | AbsE Pat (Exp a)
  | AppE (Exp a) (Exp a)
  | LetE Pat (Exp a) (Exp a)

  | BoolE Bool
  | VoidE
  | IntE Int
  | DoubleE Double
  | TupleE [Exp a]

  | Undefined
  | AsTypeOf
  | Fix
  | RunCont
  | Return
  | Then
  | CallCC deriving (Show, Typeable, Data, Functor)

instance View (Exp a) (ExpView a) where
  view (Exp _ x) = x
  updateView (Exp x _) = Exp x

data Pat
  = VarP Ident
  | TupleP [Ident] deriving (Show, Typeable, Data)

varE :: MonadReader a m => Ident -> m (Exp a)
varE x = do
  a <- ask
  return $ Exp a $ VarE x

appE :: MonadReader a m => m (Exp a) -> m (Exp a) -> m (Exp a)
appE f x = do
  a <- ask
  f' <- f
  x' <- x
  return $ Exp a $ AppE f' x'

absE :: MonadReader a m => Pat -> m (Exp a) -> m (Exp a)
absE x e = do
  a <- ask
  e' <- e
  return $ Exp a $ AbsE x e'

letE :: MonadReader a m => Pat -> m (Exp a) -> m (Exp a) -> m (Exp a)
letE x e e' = do
  a <- ask
  v <- liftM2 (LetE x) e e'
  return $ Exp a v

boolE :: MonadReader a m => Bool -> m (Exp a)
boolE bool = do
  a <- ask
  return $ Exp a $ BoolE bool

voidE :: MonadReader a m => m (Exp a)
voidE = do
  a <- ask
  return $ Exp a VoidE

intE :: MonadReader a m => Int -> m (Exp a)
intE x = do
  a <- ask
  return $ Exp a $ IntE x

doubleE :: MonadReader a m => Double -> m (Exp a)
doubleE x = do
  a <- ask
  return $ Exp a $ DoubleE x

tupleE :: MonadReader a m => [m (Exp a)] -> m (Exp a)
tupleE x = do
  a <- ask
  x' <- sequence x
  return $ Exp a $ TupleE x'

undefined' :: MonadReader a m => m (Exp a)
undefined' = do
  a <- ask
  return $ Exp a Undefined

asTypeOf' :: MonadReader a m => m (Exp a) -> m (Exp a) -> m (Exp a)
x `asTypeOf'` y = do
  a <- ask
  appE (appE (return $ Exp a AsTypeOf) x) y

fix' :: MonadReader a m => m (Exp a) -> m (Exp a)
fix' f = do
  a <- ask
  appE (return $ Exp a Fix) f

runCont :: MonadReader a m => m (Exp a) -> m (Exp a)
runCont m = do
  a <- ask
  appE (return $ Exp a RunCont) m

return' :: MonadReader a m => m (Exp a) -> m (Exp a)
return' e = do
  a <- ask
  appE (return $ Exp a Return) e

then' :: MonadReader a m => m (Exp a) -> m (Exp a) -> m (Exp a)
m `then'` n = do
  a <- ask
  appE (appE (return $ Exp a Then) m) n

callCC :: MonadReader a m => m (Exp a) -> m (Exp a)
callCC f = do
  a <- ask
  appE (return $ Exp a CallCC) f

varP :: Ident -> Pat
varP = VarP

tupleP :: [Ident] -> Pat
tupleP = TupleP
