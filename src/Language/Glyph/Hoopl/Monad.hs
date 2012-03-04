{-# LANGUAGE ExplicitForAll, NoImplicitPrelude #-}
module Language.Glyph.Hoopl.Monad (Monad (..), (=<<)) where

import Prelude (String, error)

infixl 1 >>, >>=

class Monad m where
  (>>=) :: forall e ex x a b . m e ex a -> (a -> m ex x b) -> m e x b
  (>>) :: forall e ex x a b . m e ex a -> m ex x b -> m e x b
  return :: a -> m ex ex a
  fail :: String -> m e x a

  {-# INLINE (>>) #-}
  m >> k = m >>= \ _ -> k
  fail = error

infixr 1 =<<

(=<<) :: Monad m => (a -> m ex x b) -> m e ex a -> m e x b
f =<< x = x >>= f
