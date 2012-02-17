module Language.Glyph.Hoopl
       ( module X
       , ContFlowException (..)
       , toGraph
       , showGraph'
       ) where

import Compiler.Hoopl

import Language.Glyph.Hoopl.ToGraph as X
import Language.Glyph.Hoopl.Syntax as X
import Language.Glyph.Ident
import Language.Glyph.Syntax as X hiding (Stmt,
                                          StmtView (..),
                                          Expr,
                                          ExprView (..))

showGraph' :: Graph (Insn a) e x -> String
showGraph' = show . prettyGraph