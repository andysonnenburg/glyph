module Language.Glyph.IR.Operand
       ( Operand (..)
       , operandLattice
       ) where

import Compiler.Hoopl

import Language.Glyph.Ident
import Language.Glyph.IR.Syntax

data Operand
  = Var Ident
  | Lit Lit deriving Eq

operandLattice :: DataflowLattice (WithTopAndBot [WithTop Operand])
operandLattice =
  addPoints' "operand stack" join
  where
    join label (OldFact old) (NewFact new)
      | length old == length new =
        fmap PElem $ foldr f (NoChange, []) $ zip old new
      | otherwise =
        (SomeChange, Top)
      where
        f (old', new') (changeFlag, xs) =
          (append changeFlag changeFlag', x:xs)
          where
            (changeFlag', x) = joinOperand' label (OldFact old') (NewFact new')
            append SomeChange _ = SomeChange
            append _ SomeChange = SomeChange
            append NoChange NoChange = NoChange
    joinOperand' = extendJoinDomain joinOperand
    joinOperand _ (OldFact old) (NewFact new)
      | old == new = (NoChange, PElem new)
      | otherwise = (SomeChange, Top)