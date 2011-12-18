{-# LANGUAGE DeriveDataTypeable #-}
module Language.Glyph.HM.Syntax
       ( Exp (..)
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

import Control.Monad hiding (void)

import Data.Data

import Language.Glyph.Ident

import Prelude hiding (abs)

data Exp
  = VarE Ident
  | AbsE Pat Exp
  | AppE Exp Exp
  | LetE [Ident] Exp Exp
    
  | BoolE Bool
  | VoidE
  | IntE Int
  | DoubleE Double
  | TupleE [Exp]
    
  | Undefined
  | AsTypeOf
  | Fix
  | RunCont
  | Return
  | Then
  | CallCC deriving (Typeable, Data, Show)

data Pat
  = VarP Ident
  | TupleP [Ident] deriving (Typeable, Data, Show)

varE :: Monad m => Ident -> m Exp
varE = Prelude.return . VarE

appE :: Monad m => m Exp -> m Exp -> m Exp
appE f x = do
  f' <- f
  x' <- x
  return $ AppE f' x'

absE :: Monad m => Pat -> m Exp -> m Exp
absE x e = do
  e' <- e
  return $ AbsE x e'

letE :: Monad m => [Ident] -> m Exp -> m Exp -> m Exp
letE x e e' = do
  a <- e
  b <- e'
  return $ LetE x a b

boolE :: Monad m => Bool -> m Exp
boolE = Prelude.return . BoolE

voidE :: Monad m => m Exp
voidE = Prelude.return VoidE

intE :: Monad m => Int -> m Exp
intE = Prelude.return . IntE

doubleE :: Monad m => Double -> m Exp
doubleE = return . DoubleE

tupleE :: Monad m => [m Exp] -> m Exp
tupleE x = do
  x' <- sequence x
  return $ TupleE x'

undefined' :: Monad m => m Exp
undefined' = return Undefined

asTypeOf' :: Monad m => m Exp -> m Exp -> m Exp
x `asTypeOf'` y = appE (appE (return AsTypeOf) x) y

fix' :: Monad m => m Exp -> m Exp
fix' f = appE (return Fix) f

runCont :: Monad m => m Exp -> m Exp
runCont m = appE (return RunCont) m

return' :: Monad m => m Exp -> m Exp
return' a = appE (return Return) a

then' :: Monad m => m Exp -> m Exp -> m Exp
m `then'` n = appE (appE (return Then) m) n

callCC :: Monad m => m Exp -> m Exp
callCC f = appE (return CallCC) f

varP :: Ident -> Pat
varP = VarP

tupleP :: [Ident] -> Pat
tupleP = TupleP