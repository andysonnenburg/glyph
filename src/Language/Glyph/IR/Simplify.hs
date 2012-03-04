{-# LANGUAGE GADTs, PatternGuards, ScopedTypeVariables #-}
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
    go (Stmt a (IfS x true false)) fact
      | Just (PElem (BoolL bool)) <- Map.lookup x fact =
        let stmt = mkMiddle $ Stmt a (ExprS x)
            goto = mkLast $ Goto $ if bool then true else false
        in return $ Just $ stmt <*> goto
    go (Expr a x (NotE y) labels) fact
      | Just (PElem (BoolL bool)) <- Map.lookup y fact =
        return $ Just $ mkStmt (Expr a x (LitE . BoolL . not $ bool)) labels
    go _ _ =
      return Nothing

    mkStmt :: (MaybeC x (Label, Label) -> Stmt a O x) ->
              MaybeC x (Label, Label) ->
              Graph (Stmt a) O x
    mkStmt f (JustC labels) = mkLast $ f (JustC labels)
    mkStmt f NothingC = mkMiddle $ f NothingC
