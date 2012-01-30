module Language.Glyph.Hoopl.ConstProp
       ( constProp
       ) where

type ConstFact = Map Var (WithTop Lit)

constLattice :: DataflowLattice ConstFact
constLattice =
  DataflowLattice { 