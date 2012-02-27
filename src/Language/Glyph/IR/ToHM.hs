{-# LANGUAGE ExistentialQuantification #-}
module Language.Glyph.IR.ToHM
       ( toHM
       ) where

import Compiler.Hoopl

import Language.Glyph.Hoopl
import Language.Glyph.HM.Syntax
import Language.Glyph.IR.Syntax

toHM :: Graph (Insn a) O C -> Exp a
toHM g = foldGraphNodesR f [] `seq` undefined
  where
    f x xs = SomeInsn x : xs

data SomeInsn a = forall e x . SomeInsn { unSomeInsn :: Insn a e x }
{-
toExp insns =
  runReaderT' (mconcat $ map extract insns) $
  runCont $ callCC $ do
    cc <- freshIdent
    runWithIdentT' cc $ absE (varP cc) (blockToExpr stmts)
-}