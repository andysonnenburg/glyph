{-# LANGUAGE
    DeriveDataTypeable
  , GADTs
  , ScopedTypeVariables
  , StandaloneDeriving #-}
module Language.Glyph.IR.Syntax
       ( module X
       , Fun (..)
       , Insn (..)
       , WrappedInsn (..)
       , Stmt (..)
       , Expr (..)
       , ExprIdent
       , Successor
       , mapGraph'
       , mapGraph''
       , prettyGraph
       ) where

import Compiler.Hoopl

import Data.Data
import Data.Maybe

import Language.Glyph.Hoopl
import Language.Glyph.Ident
import Language.Glyph.Pretty
import Language.Glyph.Syntax as X (MethodName,
                                   Lit (..),
                                   Name,
                                   NameView,
                                   ident,
                                   prettyText)

data Fun a = Fun Ident Params (Insns a) Vars (Funs a) deriving Typeable
type Params = [Ident]
type Insns a = Graph (Insn a) O C
type Vars = [Ident]
type Funs a = [Fun a]

instance Show (Fun a) where
  show = showDefault

instance Pretty (Fun a) where
  pretty (Fun x params insns vars funs) =
    text "fn" <+> pretty x <> tupled (map pretty params) <+> lbrace <>
    (enclose linebreak linebreak . indent 2 $
     vcat (map prettyVar vars <>
           map pretty funs <>
           [prettyGraph insns])) <>
    rbrace
    where
      prettyVar var = text "var" <+> pretty var <> semi

instance Functor Fun where
  fmap f = go
    where
      go (Fun x params insns vars funs) =
        Fun x params (mapGraph' f insns) vars (map go funs)

data Insn a e x where
  Stmt :: a -> Stmt x -> Insn a O x
  Expr :: a -> ExprIdent -> Expr -> MaybeC x (Label, Label) -> Insn a O x
  Label :: Label -> Insn a C O
  Catch :: ExprIdent -> Label -> Insn a C O
  ReturnVoid :: Insn a O C

instance Show (Insn a e x) where
  show = showDefault

deriving instance Typeable3 Insn

mapGraph' :: (a -> b) -> Graph (Insn a) e x -> Graph (Insn b) e x
mapGraph' f = mapGraph (unwrapInsn . fmap f . WrapInsn)

mapGraph'' :: forall a . (Graph (Insn a) O C -> Graph (Insn a) O C) -> Fun a -> Fun a
mapGraph'' f = go
  where
    go (Fun x params insns vars funs) =
      Fun x params (f insns) vars (map go funs)

newtype WrappedInsn e x a
  = WrapInsn { unwrapInsn :: Insn a e x
             }

instance Functor (WrappedInsn e x) where
  fmap (f :: a -> b) = WrapInsn . go . unwrapInsn
    where
      go :: forall e x . Insn a e x -> Insn b e x
      go (Stmt a x) = Stmt (f a) x
      go (Expr a x expr successors') = Expr (f a) x expr successors'
      go (Label label) = Label label
      go (Catch x label) = Catch x label
      go ReturnVoid = ReturnVoid

data Stmt x where
  ExprS :: ExprIdent -> MaybeC x (Label, Label) -> Stmt x
  ReturnS :: ExprIdent -> Successor -> Stmt C
  GotoS :: Label -> Successor -> Stmt C
  IfS :: ExprIdent -> Label -> Label -> Successor -> Stmt C
  ThrowS :: ExprIdent -> Successor -> Stmt C

deriving instance Typeable1 Stmt

data Expr where
  LitE :: Lit -> Expr
  NotE :: ExprIdent -> Expr
  VarE :: Ident -> Expr
  ApplyE :: ExprIdent -> [ExprIdent] -> Expr
  ApplyMethodE :: ExprIdent -> MethodName -> [ExprIdent] -> Expr
  AssignE :: Ident -> ExprIdent -> Expr

deriving instance Typeable Expr

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
        text "let" <+> pretty x <+> char '=' <+> pretty expr
        `prettySuccessors`
        successors'
      go (Label label) =
        prettyLabel label <> colon
      go (Catch x label) =
        prettyLabel label <> colon <+>
        text "catch" <+> parens (pretty x) <> semi
      go ReturnVoid =
        text "return" <+> pretty VoidL <> semi

instance Pretty (Stmt x) where
  pretty = go
    where
      go :: Stmt x -> Doc e
      go (ExprS expr successors') =
        pretty expr
        `prettySuccessors`
        successors'
      go (ReturnS expr successor) =
        text "return" <+> pretty expr
        `prettySuccessor`
        successor
      go (GotoS label successor) =
        prettyGoto label
        `prettySuccessor`
        successor
      go (IfS expr then' else' successor) =
        text "if" <+> parens (pretty expr) <+>
        prettyLabel then' <+>
        prettyLabel else'
        `prettySuccessor`
        successor
      go (ThrowS expr successor) =
        text "throw" <+> pretty expr
        `prettySuccessor`
        successor

prettyGoto :: Label -> Doc e
prettyGoto label =
  text "goto" <+> prettyLabel label

instance Pretty Expr where
  pretty = go
    where
      go (LitE lit) =
        pretty lit
      go (NotE x) =
        char '!' <> pretty x
      go (VarE name) =
        pretty name
      go (ApplyE expr exprs) =
        pretty expr <> tupled (map pretty exprs)
      go (ApplyMethodE expr methodName exprs) =
        pretty expr <>
        char '.' <>
        prettyText methodName <>
        tupled (map pretty exprs)
      go (AssignE name x) =
        pretty name <+> char '=' <+> pretty x

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

prettyLabel :: Label -> Doc e
prettyLabel = text . show

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
          go :: Stmt C -> [Label]
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
