{-# LANGUAGE
    DeriveDataTypeable
  , GADTs
  , ScopedTypeVariables
  , StandaloneDeriving #-}
module Language.Glyph.IR.Syntax
       ( module X
       , Insn (..)
       , WrappedInsn (..)
       , Stmt (..)
       , WrappedStmt (..)
       , Expr (..)
       , ExprIdent
       , Successor
       , mapGraph'
       , mapGraph''
       , prettyGraph
       ) where

import Compiler.Hoopl

import Data.Data
import Data.Foldable (Foldable, toList)
import Data.Maybe

import Language.Glyph.Hoopl
import Language.Glyph.Ident
import Language.Glyph.Syntax as X (MethodName,
                                   Lit (..),
                                   Name,
                                   NameView,
                                   ident,
                                   prettyText)

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

data Insn a e x where
  Stmt :: a -> Stmt a x -> Insn a O x
  Expr :: a -> ExprIdent -> Expr a -> MaybeC x (Label, Label) -> Insn a O x
  Label :: Label -> Insn a C O
  Catch :: ExprIdent -> Label -> Insn a C O
  ReturnVoid :: Insn a O C

instance Show (Insn a e x) where
  show = show . pretty

deriving instance Typeable3 Insn

mapGraph' :: (a -> b) -> Graph (Insn a) e x -> Graph (Insn b) e x
mapGraph' f = mapGraph (unwrapInsn . fmap f . WrapInsn)

mapGraph'' :: forall a .
              (Graph (Insn a) O C -> Graph (Insn a) O C) ->
              Graph (Insn a) O C ->
              Graph (Insn a) O C
mapGraph'' f = f . go
  where
    go = mapGraph f'
    f' :: forall e x . Insn a e x -> Insn a e x
    f' (Stmt a (FunDeclS name params graph)) =
      Stmt a (FunDeclS name params (f (go graph)))
    f' (Expr a x (FunE x' params graph) successors') =
      Expr a x (FunE x' params (f (go graph))) successors'
    f' insn =
      insn

newtype WrappedInsn e x a
  = WrapInsn { unwrapInsn :: Insn a e x
             }

instance Functor (WrappedInsn e x) where
  fmap (f :: a -> b) = WrapInsn . go . unwrapInsn
    where
      go :: forall e x . Insn a e x -> Insn b e x
      go (Stmt a x) = Stmt (f a) (unwrapStmt . fmap f . WrapStmt $ x)
      go (Expr a x expr successors') = Expr (f a) x (fmap f expr) successors'
      go (Label label) = Label label
      go (Catch x label) = Catch x label
      go ReturnVoid = ReturnVoid

data Stmt a x where
  ExprS :: ExprIdent -> MaybeC x (Label, Label) -> Stmt a x
  VarDeclS :: Name -> Stmt a O
  FunDeclS :: Name -> [Name] -> Graph (Insn a) O C -> Stmt a O
  ReturnS :: ExprIdent -> Successor -> Stmt a C
  GotoS :: Label -> Successor -> Stmt a C
  IfS :: ExprIdent -> Label -> Label -> Successor -> Stmt a C
  ThrowS :: ExprIdent -> Successor -> Stmt a C

deriving instance Typeable2 Stmt

newtype WrappedStmt x a
  = WrapStmt { unwrapStmt :: Stmt a x
             }

instance Functor (WrappedStmt x) where
  fmap f = WrapStmt . go . unwrapStmt
    where
      go (ExprS x successors') = ExprS x successors'
      go (VarDeclS name) = VarDeclS name
      go (FunDeclS name params graph) = FunDeclS name params (mapGraph' f graph)
      go (ReturnS x successor) = ReturnS x successor
      go (GotoS label successor) = GotoS label successor
      go (IfS x then' else' successor) = IfS x then' else' successor
      go (ThrowS x successor) = ThrowS x successor

data Expr a where
  LitE :: Lit -> Expr a
  NotE :: ExprIdent -> Expr a
  VarE :: Name -> Expr a
  FunE :: Ident -> [Name] -> Graph (Insn a) O C -> Expr a
  ApplyE :: ExprIdent -> [ExprIdent] -> Expr a
  ApplyMethodE :: ExprIdent -> MethodName -> [ExprIdent] -> Expr a
  AssignE :: Name -> ExprIdent -> Expr a

deriving instance Typeable1 Expr

instance Functor Expr where
  fmap f = go
    where
      go (LitE lit) = LitE lit
      go (NotE x) = NotE x
      go (VarE name) = VarE name
      go (FunE x params graph) = FunE x params (mapGraph' f graph)
      go (ApplyE x xs) = ApplyE x xs
      go (ApplyMethodE x y xs) = ApplyMethodE x y xs
      go (AssignE name x) = AssignE name x

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

instance Pretty (Stmt a x) where
  pretty = go
    where
      go :: Stmt a x -> Doc e
      go (ExprS expr successors') =
        pretty expr
        `prettySuccessors`
        successors'
      go (VarDeclS name) =
        text "var" <+> pretty name <> semi
      go (FunDeclS name params graph) =
        text "fn" <+> pretty name <> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
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

instance Pretty (Expr a) where
  pretty = go
    where
      go (LitE lit) =
        pretty lit
      go (NotE x) =
        char '!' <> pretty x
      go (VarE name) =
        pretty name
      go (FunE _ params graph) =
        text "fn" <+> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
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
