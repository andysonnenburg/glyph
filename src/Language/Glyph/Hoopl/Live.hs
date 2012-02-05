{-# LANGUAGE GADTs, ScopedTypeVariables, ViewPatterns #-}
module Language.Glyph.Hoopl.Live
       ( liveLattice
       , liveness
       , deadAssignElim
       ) where

import Compiler.Hoopl

import Language.Glyph.Hoopl.Syntax
import Language.Glyph.IdentSet (IdentSet)
import qualified Language.Glyph.IdentSet as Set
import Language.Glyph.Monoid

type Live = IdentSet

liveLattice :: DataflowLattice Live
liveLattice =
  DataflowLattice { fact_name = "live variables"
                  , fact_bot = Set.empty
                  , fact_join = join
                  }
  where
    join _ (OldFact old) (NewFact new) = (change, fact)
      where
        fact = new <> old
        change = changeIf $ Set.size fact > Set.size old

liveness :: BwdTransfer (Stmt a) Live
liveness = mkBTransfer transfer
  where
    transfer :: Stmt a e x -> Fact x Live -> Live
    transfer = undefined

deadAssignElim :: forall m a . FuelMonad m => BwdRewrite m (Stmt a) Live
deadAssignElim = mkBRewrite rewrite
  where
    rewrite :: Stmt a e x -> Fact x Live -> m (Maybe (Graph (Stmt a) e x))
    rewrite = undefined
