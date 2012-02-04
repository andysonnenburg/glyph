{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Language.Glyph.Hoopl.Simplify
       ( simplify
       ) where

import Compiler.Hoopl

import Language.Glyph.Hoopl.ConstProp
import Language.Glyph.Hoopl.Syntax
import qualified Language.Glyph.IdentMap as Map

simplify :: forall m a . FuelMonad m => FwdRewrite m (Stmt a) ConstFact
simplify = deepFwdRw go
  where
    go :: Stmt a e x -> ConstFact -> m (Maybe (Graph (Stmt a) e x))
    go (Stmt _ (IfS x true false)) fact =
      return $
      case Map.lookup x fact of
        Just (PElem (BoolL bool)) ->
          Just $ mkLast $ Goto $ if bool then true else false
        _ ->
          Nothing
    go _ _ =
      return Nothing