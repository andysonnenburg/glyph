{-# LANGUAGE ScopedTypeVariables #-}
module Language.Glyph.Hoopl.Simplify
       ( simplify
       ) where

import Compiler.Hoopl

import Language.Glyph.Hoopl.Syntax

simplify :: forall m f a . FuelMonad m => FwdRewrite m (Stmt a) f
simplify = deepFwdRw rewrite
  where
    rewrite :: Stmt a e x -> f -> m (Maybe (Graph (Stmt a) e x))
    rewrite _ _ =
      return Nothing