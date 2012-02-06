{-# LANGUAGE GADTs, PatternGuards, ScopedTypeVariables, ViewPatterns #-}
module Language.Glyph.Hoopl.ConstProp
       ( ConstFact
       , constLattice
       , identIsLit
       , constProp
       , initConstFact
       ) where

import Compiler.Hoopl hiding (joinMaps)

import Data.Maybe

import Language.Glyph.Hoopl.Syntax
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as Map

import Prelude hiding (elem, last)

type ConstFact = IdentMap (WithTop Lit)

botConstFact :: ConstFact
botConstFact = Map.empty

constLattice :: DataflowLattice ConstFact
constLattice =
  DataflowLattice { fact_name = "const var value"
                  , fact_bot = botConstFact
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

identIsLit :: FwdTransfer (Stmt a) ConstFact
identIsLit = mkFTransfer go
  where
    go :: Stmt a e x -> ConstFact -> Fact x ConstFact
    go (Stmt _ (ExprS _)) fact =
      fact
    go (Stmt _ (VarDeclS (ident -> x) (Just y))) fact
      | Just (PElem lit) <- Map.lookup y fact =
        Map.insert x (PElem lit) fact
    go (Stmt _ (VarDeclS {})) fact =
      fact
    go (Stmt _ (FunDeclS {})) fact =
      fact
    go (Stmt _ (ReturnS {})) _ =
      mapEmpty
    go (Stmt _ (IfS x true false)) fact =
      mkFactBase constLattice
      [ (true, Map.insert x (PElem (BoolL True)) fact)
      , (false, Map.insert x (PElem (BoolL False)) fact)
      ]
    go (Stmt _ (ThrowS _ Nothing)) _ =
      mapEmpty
    go (Stmt _ (ThrowS _ (Just catchLabel))) fact =
      mapSingleton catchLabel fact
    go (Expr _ x v NothingC) fact =
      fromExpr x v fact
    go (Expr _ x v (JustC (nextLabel, catchLabel))) fact =
      mkFactBase constLattice
      [ (nextLabel, fromExpr x v fact)
      , (catchLabel, fact)
      ]
    go (Label _) fact =
      fact
    go (Goto label) fact =
      mapSingleton label fact
    go (Catch x _) fact =
      Map.insert x Top fact
    go ReturnVoid _ =
      mapEmpty

    fromExpr :: ExprIdent -> ExprView a -> ConstFact -> ConstFact
    fromExpr x (LitE lit) fact =
      Map.insert x (PElem lit) fact
    fromExpr x (AssignE (ident -> y) z) fact
      | Just elem@(PElem _) <- Map.lookup z fact =
        Map.insert x elem (Map.insert y elem fact)
      | otherwise =
        Map.insert x Top (Map.insert y Top fact)
    fromExpr x (VarE (ident -> y)) fact
      | Just (PElem lit) <- Map.lookup y fact =
        Map.insert x (PElem lit) fact
    fromExpr x _ fact =
      Map.insert x Top fact

constProp :: forall m a . FuelMonad m => FwdRewrite m (Stmt a) ConstFact
constProp = mkFRewrite go
  where
    go :: Stmt a e x -> ConstFact -> m (Maybe (Graph (Stmt a) e x))
    go (Expr a x (VarE (ident -> y)) labels) fact
      | Just lit <- getLit y fact =
        return $ Just $ mkStmt (Expr a x (LitE lit)) labels
    go _ _ =
      return Nothing

    getLit :: Ident -> ConstFact -> Maybe Lit
    getLit x fact
      | Just (PElem lit) <- Map.lookup x fact = Just lit
      | otherwise = Nothing

    mkStmt :: (MaybeC x (Label, Label) -> Stmt a O x) ->
              MaybeC x (Label, Label) ->
              Graph (Stmt a) O x
    mkStmt f (JustC labels) = mkLast $ f (JustC labels)
    mkStmt f NothingC = mkMiddle $ f NothingC

initConstFact :: [Ident] -> ConstFact
initConstFact params = Map.fromList [(param, Top) | param <- params]
