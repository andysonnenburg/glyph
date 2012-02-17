{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Language.Glyph.IR.Syntax
       ( module X
       , Insn (..)
       , InsnView (..)
       , Successor
       , Successors
       , prettyGraph
       ) where

import Compiler.Hoopl

import Data.Foldable (Foldable, toList)
import Data.Maybe

import Language.Glyph.Ident
import Language.Glyph.Syntax as X (Lit (..),
                                   Name,
                                   NameView,
                                   ident,
                                   prettyNameView)

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

data Insn a e x where
  Insn :: a -> InsnView a x -> Insn a O x
  Label :: Label -> Insn a C O
  Catch :: Label -> Insn a C O
  ReturnVoid :: Insn a O C

data InsnView a x where
  VarDecl :: Name -> InsnView a O
  FunDecl :: Name -> [Name] -> Graph (Insn a) O C -> InsnView a O
  Fun :: Ident -> [Name] -> Graph (Insn a) O C -> Successors x -> InsnView a x
  Return :: Successor -> InsnView a C
  Goto :: Label -> Successor -> InsnView a C
  If :: Label -> Label -> Successor -> InsnView a C
  Throw :: Successor -> InsnView a C
  Load :: Name -> Successors x -> InsnView a x
  Store :: Name -> Successors x -> InsnView a x
  Invoke :: Int -> Successors x -> InsnView a x
  Lit :: Lit -> Successors x -> InsnView a x
  Not :: Successors x -> InsnView a x
  Dup :: Successors x -> InsnView a x
  Pop :: Successors x -> InsnView a x

type Successors x = MaybeC x (Label, Label)

type Successor = Maybe Label

instance NonLocal (Insn a) where
  entryLabel = go
    where
      go (Label label) = label
      go (Catch label) = label
  
  successors = insnSuccessors
    where
      insnSuccessors = go
        where
          go (Insn _ x) =
            insnViewSuccessors x
          go ReturnVoid =
            []
      
      insnViewSuccessors = go
        where
          go :: InsnView a C -> [Label]
          go (Fun _ _params _graph (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (Return maybeCatchLabel) =
            maybeToList maybeCatchLabel
          go (Goto label maybeCatchLabel) =
            label : maybeToList maybeCatchLabel
          go (If thenLabel elseLabel maybeCatchLabel) =
            [thenLabel, elseLabel] ++ maybeToList maybeCatchLabel
          go (Throw maybeCatchLabel) =
            maybeToList maybeCatchLabel
          go (Load _ (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (Store _ (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (Invoke _ (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (Lit _ (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (Not (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (Dup (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (Pop (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]

instance Pretty (Insn a e x) where
  pretty = go
    where
      go :: Insn a e x -> Doc e'
      go (Insn _ x) =
        pretty x
      go (Label label) =
        prettyLabel label <> colon
      go (Catch label) =
        prettyLabel label <> colon <+> text "catch" <> semi
      go ReturnVoid =
        text "return" <+> pretty VoidL <> semi
      
instance Pretty (InsnView a x) where
  pretty = go
    where
      go (VarDecl name) =
        text "var" <+> pretty name <> semi
      go (FunDecl name params graph) =
        text "fn" <+> pretty name <> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (Fun _ params graph successors') =
        text "fn" <+> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
        `prettySuccessors`
        successors'
      go (Return successor) =
        text "return"
        `prettySuccessor`
        successor
      go (Goto label successor) =
        text "goto" <+> prettyLabel label
        `prettySuccessor`
        successor
      go (If thenLabel elseLabel successor) =
        text "if" <+>
        prettyLabel thenLabel <+>
        prettyLabel elseLabel
        `prettySuccessor`
        successor
      go (Throw successor) =
        text "throw" `prettySuccessor` successor <> semi
      go (Load name successors') =
        text "load" <+> pretty name
        `prettySuccessors`
        successors'
      go (Store name successors') =
        text "store" <+> pretty name
        `prettySuccessors`
        successors'
      go (Invoke arity successors') =
        text "invoke" <> char '/' <> text (show arity)
        `prettySuccessors`
        successors'
      go (Lit lit successors') =
        text "ldc" <+> pretty lit
        `prettySuccessors`
        successors'
      go (Not successors') =
        char '!'
        `prettySuccessors`
        successors'
      go (Dup successors') =
        text "dup"
        `prettySuccessors`
        successors'
      go (Pop successors') =
        text "pop"
        `prettySuccessors`
        successors'

prettyGoto :: Label -> Doc e
prettyGoto label =
  text "goto" <+> prettyLabel label <> semi

prettySuccessors :: Doc e -> Successors x -> Doc e
prettySuccessors = go
  where
    go doc NothingC =
      doc <> semi
    go doc (JustC (nextLabel, catchLabel)) =
      doc <+> text "unwind" <+> prettyLabel catchLabel <> semi
      `above`
      prettyGoto nextLabel <> semi

prettySuccessor :: Doc e -> Successor -> Doc e
prettySuccessor = go
  where
    go doc (Just catchLabel) =
      doc <+> text "unwind" <+> prettyLabel catchLabel <> semi
    go doc Nothing =
      doc <> semi

prettyLabel :: Label -> Doc e
prettyLabel = text . show

prettyGraph :: Graph (Insn a) e x -> Doc e'
prettyGraph = go
  where
    go :: Graph (Insn a) e x -> Doc e'
    go GNil =
      empty
    go (GUnit unit) =
      vcat $ block unit []
    go (GMany entry blocks exit) =
      vcat $
      open (flip block []) entry ++
      body blocks ++
      open (flip block []) exit

    open :: (a -> [Doc e]) -> MaybeO z a -> [Doc e]
    open _ NothingO = []
    open p (JustO x) = p x

    body :: LabelMap (Block (Insn a) C C) -> [Doc e]
    body blocks =
      concatMap (flip block []) . mapElems $ blocks

    block :: forall a e x e' .
             Block (Insn a) e x ->
             IndexedCO x [Doc e'] [Doc e'] ->
             IndexedCO e [Doc e'] [Doc e']
    block = foldBlockNodesB f
      where
        f :: forall e x . Insn a e x -> [Doc e'] -> [Doc e']
        f stmt docs = pretty stmt : docs

tupled :: Foldable f => f (Doc e) -> Doc e
tupled = encloseSep lparen rparen (comma <> space)

encloseSep :: Foldable f => Doc e -> Doc e -> Doc e -> f (Doc e) -> Doc e
encloseSep left right sp ds0 =
  case toList ds0 of
    [] -> left <> right
    [d] -> left <> d <> right
    ds -> left <> align (cat (zipWith (<>) (init ds) (repeat sp) ++ [last ds <> right]))