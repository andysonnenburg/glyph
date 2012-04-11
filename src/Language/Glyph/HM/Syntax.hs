{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFunctor
  , MultiParamTypeClasses
  , ViewPatterns #-}
module Language.Glyph.HM.Syntax
       ( module X
       , Exp (..)
       , ExpView (..)
       , Lit (..)
       , varE
       , appE
       , absE
       , letE
       , litE
       , mkTuple
       , tupleE
       , select
       , access
       , accessE
       , undefined'
       , asTypeOf'
       , fix'
       , runCont
       , callCC
       , return'
       , then'
       ) where

import Control.Monad
import Control.Monad.Reader

import Data.Data

import Language.Glyph.Ident
import Language.Glyph.Syntax (Lit (..), prettyText)
import Language.Glyph.Type as X (Label)
import Language.Glyph.View

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

class Pretty a => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc e
  prettyPrec _ = pretty

prettyDefault :: PrettyPrec a => a -> Doc e
prettyDefault = prettyPrec 0

prettyParen :: Bool -> Doc e -> Doc e
prettyParen b p = if b then parens p else p

data Exp a = Exp a (ExpView a) deriving (Typeable, Data, Functor)

instance Pretty (Exp a) where
  pretty = prettyDefault

instance PrettyPrec (Exp a) where
  prettyPrec p = prettyPrec p . view

instance Show (Exp a) where
  show = show . pretty

data ExpView a
  = VarE Ident
  | AbsE Ident (Exp a)
  | AppE (Exp a) (Exp a)
  | LetE Ident (Exp a) (Exp a)

  | LitE Lit

  | MkTuple Int
  | Select Int Int
  | Access Label
  | Undefined
  | AsTypeOf
  | Fix
  | RunCont
  | Return
  | Then
  | CallCC deriving (Typeable, Data, Functor)

instance Show (ExpView a) where
  show = show . pretty

instance Pretty (ExpView a) where
  pretty = prettyDefault

instance PrettyPrec (ExpView a) where
  prettyPrec p = go
    where
      go (VarE x) =
        pretty x
      go (AbsE x e) =
        prettyParen (p > 0) $
        nest 2 $
        char '\\' <> pretty x <> char '.'
        `above`
        pretty e
      go (AppE (view -> AppE (view -> AsTypeOf) e1) e2) =
        prettyParen (p > 10) $
        prettyPrec 11 e1 <+> text "`asTypeOf`" <+> prettyPrec 11 e2
      go (AppE (view -> AppE (view -> Then) e1) e2) =
        prettyParen (p > 1) $
        align $
        prettyPrec 2 e1 <+> text ">>"
        `above`
        prettyPrec 2 e2
      go (AppE e1 e2) =
        prettyParen (p > appPrec) $
        prettyPrec appPrec1 e1 <+> prettyPrec appPrec1 e2
      go (LetE x e1 e2) =
        text "let" <+> pretty x <+> char '=' <+> align (pretty e1)
        `above`
        text "in" <+> align (pretty e2)
      go (LitE lit) =
        pretty lit
      go (MkTuple 0) =
        text "()"
      go (MkTuple x) =
        text "mkTuple" <> char '_' <> pretty x
      go (Select a b) =
        text "select" <> char '_' <> pretty a <> char '_' <> pretty b
      go (Access l) =
        char '.' <> prettyText l
      go Undefined =
        text "undefined"
      go AsTypeOf =
        text "asTypeOf"
      go Fix =
        text "fix"
      go RunCont =
        text "runCont"
      go Return =
        text "return"
      go Then =
        text "then"
      go CallCC =
        text "callCC"

appPrec :: Int
appPrec = 10

appPrec1 :: Int
appPrec1 = 11

instance View (Exp a) (ExpView a) where
  view (Exp _ x) = x

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

absE :: MonadReader a m => Ident -> m (Exp a) -> m (Exp a)
absE x e = do
  a <- ask
  e' <- e
  return $ Exp a $ AbsE x e'

letE :: MonadReader a m => Ident -> m (Exp a) -> m (Exp a) -> m (Exp a)
letE x e e' = do
  a <- ask
  v <- liftM2 (LetE x) e e'
  return $ Exp a v

litE :: MonadReader a m => Lit -> m (Exp a)
litE lit = do
  a <- ask
  return $ Exp a $ LitE lit

mkTuple :: MonadReader a m => Int -> m (Exp a)
mkTuple x = do
  a <- ask
  return $ Exp a $ MkTuple x

tupleE :: MonadReader a m => [m (Exp a)] -> m (Exp a)
tupleE es = foldl appE (mkTuple l) es
  where
    l = length es

select :: MonadReader a m => Int -> Int -> m (Exp a)
select x y = do
  a <- ask
  return $ Exp a $ Select x y

access :: MonadReader a m => Label -> m (Exp a)
access l = do
  a <- ask
  return $ Exp a $ Access l

accessE :: MonadReader a m => Label -> m (Exp a) -> m (Exp a)
accessE = appE . access

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
