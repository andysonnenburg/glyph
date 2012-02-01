{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Language.Glyph.Hoopl.ConstProp
       ( ConstFact
       , constLattice
       , varIsLit
       , constProp
       ) where

import Compiler.Hoopl hiding (joinMaps)

import Data.Maybe

import Language.Glyph.Hoopl.Syntax
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as Map

type ConstFact = IdentMap (WithTop Lit)

constLattice :: DataflowLattice ConstFact
constLattice =
  DataflowLattice { fact_name = "const var value"
                  , fact_bot = Map.empty
                  , fact_join = join
                  }
  where
    join = joinMaps $ extendJoinDomain constFactAdd
    constFactAdd _ (OldFact old) (NewFact new)
      | old == new = (NoChange, PElem new)
      | otherwise = (SomeChange, Top)

joinMaps :: JoinFun v -> JoinFun (IdentMap v)
joinMaps elemJoin label (OldFact old) (NewFact new) =
  Map.foldrWithKey f (NoChange, old) new
  where
    f k new' (change, joinMap) =
      maybe nothing just $ Map.lookup k joinMap
      where
        nothing =
          (SomeChange, Map.insert k new' joinMap)
        just old' =
          case elemJoin label (OldFact old') (NewFact new') of
            (SomeChange, v) -> (SomeChange, Map.insert k v joinMap)
            (NoChange, _) -> (change, joinMap)

varIsLit :: FwdTransfer (Stmt a) ConstFact
varIsLit = mkFTransfer3 first middle last
  where
    first _ fact = fact
    middle _ fact = fact
    last x fact = mapFromList [(successor, fact) | successor <- successors x]

constProp :: forall m a . FuelMonad m => FwdRewrite m (Stmt a) ConstFact
constProp = mkFRewrite rewrite
  where
    rewrite :: Stmt a e x -> ConstFact -> m (Maybe (Graph (Stmt a) e x))
    rewrite _ _ =
      return Nothing
