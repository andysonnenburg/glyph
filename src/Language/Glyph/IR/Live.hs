{-# LANGUAGE GADTs, ScopedTypeVariables, ViewPatterns #-}
module Language.Glyph.Hoopl.Live
       ( LiveFact
       , liveLattice
       , liveness
       , deadCodeElim
       , initLiveFact
       ) where

import Compiler.Hoopl

import Data.Monoid

import Language.Glyph.Hoopl.Syntax
import Language.Glyph.IdentSet (IdentSet)
import qualified Language.Glyph.IdentSet as Set

type LiveFact = IdentSet

botLiveFact :: LiveFact
botLiveFact = Set.empty

liveLattice :: DataflowLattice LiveFact
liveLattice =
  DataflowLattice { fact_name = "live variables"
                  , fact_bot = botLiveFact
                  , fact_join = join
                  }
  where
    join _ (OldFact old) (NewFact new) = (change, fact)
      where
        fact = new <> old
        change = changeIf $ Set.size fact > Set.size old

liveness :: BwdTransfer (Stmt a) LiveFact
liveness = mkBTransfer go
  where
    go :: Stmt a e x -> Fact x LiveFact -> LiveFact
    go _ _ = undefined

deadCodeElim :: forall m a . FuelMonad m => BwdRewrite m (Stmt a) LiveFact
deadCodeElim = mkBRewrite go
  where
    go :: Stmt a e x -> Fact x LiveFact -> m (Maybe (Graph (Stmt a) e x))
    go _ _ = return Nothing

initLiveFact :: LiveFact
initLiveFact = Set.empty
