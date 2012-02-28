{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Language.Glyph.IR.Syntax
       ( module X
       , Insn (..)
       , Stmt (..)
       , Expr (..)
       , ExprIdent
       , Successor
       , prettyGraph
       ) where

import Compiler.Hoopl

import Data.Foldable (Foldable, toList)
import Data.Maybe

import Language.Glyph.Hoopl
import Language.Glyph.Ident
import Language.Glyph.Syntax as X (Lit (..),
                                   Name,
                                   NameView,
                                   ident,
                                   prettyNameView)

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

data Insn a e x where
  Stmt :: a -> Stmt a x -> Insn a O x
  Expr :: a -> ExprIdent -> Expr a -> MaybeC x (Label, Label) -> Insn a O x
  Label :: Label -> Insn a C O
  Catch :: ExprIdent -> Label -> Insn a C O
  ReturnVoid :: Insn a O C

instance Show (Insn a e x) where
  show = show . pretty

data Stmt a x where
  ExprS :: ExprIdent -> MaybeC x (Label, Label) -> Stmt a x
  VarDeclS :: Name -> Stmt a O
  FunDeclS :: Name -> [Name] -> Graph (Insn a) O C -> Stmt a O
  ReturnS :: ExprIdent -> Successor -> Stmt a C
  GotoS :: Label -> Successor -> Stmt a C
  IfS :: ExprIdent -> Label -> Label -> Successor -> Stmt a C
  ThrowS :: ExprIdent -> Successor -> Stmt a C

data Expr a where
  LitE :: Lit -> Expr a
  NotE :: ExprIdent -> Expr a
  VarE :: Name -> Expr a
  FunE :: Ident -> [Name] -> Graph (Insn a) O C -> Expr a
  ApplyE :: ExprIdent -> [ExprIdent] -> Expr a
  AssignE :: Name -> ExprIdent -> Expr a

type ExprIdent = Ident

type Successor = Maybe Label

prettyGraph :: Graph (Insn a) e x -> Doc e'
prettyGraph = vcat . foldGraphNodesR f []
  where
    f :: Insn a e x -> [Doc e'] -> [Doc e']
    f insn docs = pretty insn : docs

instance Pretty (Insn a e x) where
  pretty = go
    where
      go :: Insn a e x -> Doc e'
      go (Stmt _ x) =
        pretty x
      go (Expr _ x expr successors') =
        text "let" <+> prettyIdent x <+> char '=' <+> pretty expr
        `prettySuccessors`
        successors'
      go (Label label) =
        prettyLabel label <> colon
      go (Catch x label) =
        prettyLabel label <> colon <+>
        text "catch" <+> parens (prettyIdent x) <> semi
      go ReturnVoid =
        text "return" <+> pretty VoidL <> semi

instance Pretty (Stmt a x) where
  pretty = go
    where
      go :: Stmt a x -> Doc e
      go (ExprS expr successors') =
        prettyIdent expr
        `prettySuccessors`
        successors'
      go (VarDeclS name) =
        text "var" <+> pretty name <> semi
      go (FunDeclS name params graph) =
        text "fn" <+> pretty name <> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (ReturnS expr successor) =
        text "return" <+> prettyIdent expr
        `prettySuccessor`
        successor
      go (GotoS label successor) =
        prettyGoto label
        `prettySuccessor`
        successor
      go (IfS expr then' else' successor) =
        text "if" <+> parens (prettyIdent expr) <+>
        prettyLabel then' <+>
        prettyLabel else'
        `prettySuccessor`
        successor
      go (ThrowS expr successor) =
        text "throw" <+> prettyIdent expr
        `prettySuccessor`
        successor

prettyGoto :: Label -> Doc e
prettyGoto label =
  text "goto" <+> prettyLabel label

instance Pretty (Expr a) where
  pretty = go
    where
      go (LitE lit) =
        pretty lit
      go (NotE x) =
        char '!' <> prettyIdent x
      go (VarE name) =
        pretty name
      go (FunE _ params graph) =
        text "fn" <+> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (ApplyE expr exprs) =
        prettyIdent expr <> tupled (map prettyIdent exprs)
      go (AssignE name x) =
        pretty name <+> char '=' <+> prettyIdent x

prettySuccessors :: Doc e -> MaybeC x (Label, Label) -> Doc e
prettySuccessors = go
  where
    go doc (JustC (nextLabel, catchLabel)) =
      doc <+> text "unwind" <+> prettyLabel catchLabel <> semi <+>
      prettyGoto nextLabel <> semi
    go doc NothingC =
      doc <> semi

prettySuccessor :: Doc e -> Successor -> Doc e
prettySuccessor = go
  where
    go doc (Just catchLabel) =
      doc <+> text "unwind" <+> prettyLabel catchLabel <> semi
    go doc Nothing =
      doc <> semi

prettyIdent :: Ident -> Doc e
prettyIdent = text . show

prettyLabel :: Label -> Doc e
prettyLabel = text . show

tupled :: Foldable f => f (Doc e) -> Doc e
tupled = encloseSep lparen rparen (comma <> space)

encloseSep :: Foldable f => Doc e -> Doc e -> Doc e -> f (Doc e) -> Doc e
encloseSep left right sp ds0 =
  case toList ds0 of
    [] -> left <> right
    [d] -> left <> d <> right
    ds -> left <> align (cat (zipWith (<>) (init ds) (repeat sp) ++ [last ds <> right]))

instance NonLocal (Insn a) where
  entryLabel = go
    where
      go :: Insn a C x -> Label
      go (Label label) = label
      go (Catch _ label) = label
  
  successors = stmtSuccessors
    where
      stmtSuccessors = go
        where
          go (Stmt _ x) =
            stmtViewSuccessors x
          go (Expr _ _ _ (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go ReturnVoid =
            []

      stmtViewSuccessors = go
        where
          go :: Stmt a C -> [Label]
          go (ExprS _ (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (GotoS label successor) =
            label : maybeToList successor
          go (ReturnS _ successor) =
            maybeToList successor
          go (IfS _ thenLabel elseLabel successor) =
            [thenLabel, elseLabel] ++ maybeToList successor
          go (ThrowS _ maybeCatchLabel') =
            maybeToList maybeCatchLabel'

