module Language.Glyph.IR
       ( module X
       , ContFlowException (..)
       , showGraph'
       ) where

import Compiler.Hoopl

import Language.Glyph.IR.ToGraph as X
import Language.Glyph.IR.Syntax as X
import Language.Glyph.Syntax as X hiding (Stmt,
                                          StmtView (..),
                                          Expr,
                                          ExprView (..))

showGraph' :: Graph (Insn a) e x -> String
showGraph' = show . prettyGraph