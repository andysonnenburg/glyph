{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFunctor
  , MultiParamTypeClasses
  , ViewPatterns #-}
module Language.Glyph.HM.Syntax
       ( Exp (..)
       , ExpView (..)
       , Pat (..)
       , Lit (..)
       , varE
       , appE
       , absE
       , letE
       , litE
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
import Data.Foldable (Foldable, toList)

import Language.Glyph.Ident
import Language.Glyph.Syntax (Lit (..))
import Language.Glyph.View

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

data Exp a = Exp a (ExpView a) deriving (Typeable, Data, Functor)

instance Pretty (Exp a) where
  pretty = pretty . view

instance Show (Exp a) where
  show = show . pretty

data ExpView a
  = VarE Ident
  | AbsE Pat (Exp a)
  | AppE (Exp a) (Exp a)
  | LetE Pat (Exp a) (Exp a)

  | LitE Lit

  | TupleE [Exp a]

  | Undefined
  | AsTypeOf
  | Fix
  | RunCont
  | Return
  | Then
  | CallCC deriving (Typeable, Data, Functor)

instance Pretty (ExpView a) where
  pretty = go
    where
      go (VarE x) =
        pretty x
      go (AbsE p e) =
        char '\\' <> pretty p <> char '.' <+> hang 2 (pretty e)
      go (AppE (view -> AppE (view -> Then) e1) e2) =
        pretty e1 <+> text ">>"
        `above`
        pretty e2
      go (AppE e1 e2) =
        pretty e1 </> hang 2 (pretty e2)
      go (LetE p e1 e2) =
        text "let" </> pretty p </> char '=' </> pretty e1 </> text "in" </> pretty e2
      go (LitE lit) =
        pretty lit
      go (TupleE x) =
        tupled (map pretty x)
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

instance View (Exp a) (ExpView a) where
  view (Exp _ x) = x

data Pat
  = VarP Ident
  | TupleP [Ident] deriving (Show, Typeable, Data)

instance Pretty Pat where
  pretty = go
    where
      go (VarP x) = pretty x
      go (TupleP x) = tupled (map pretty x)

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

litE :: MonadReader a m => Lit -> m (Exp a)
litE lit = do
  a <- ask
  return $ Exp a $ LitE lit

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

tupled :: Foldable f => f (Doc e) -> Doc e
tupled = encloseSep lparen rparen (comma <> space)

encloseSep :: Foldable f => Doc e -> Doc e -> Doc e -> f (Doc e) -> Doc e
encloseSep left right sp ds0 =
  case toList ds0 of
    [] -> left <> right
    [d] -> left <> d <> right
    ds -> left <> align (cat (zipWith (<>) (init ds) (repeat sp) ++ [last ds <> right]))
