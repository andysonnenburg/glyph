{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeFamilies
  , UndecidableInstances #-}
module Language.Glyph.IR.Syntax
       ( module X
       , Form (..)
       , MaybeL (..)
       , MaybeU (..)
       , Module (..)
       , Object (..)
       , Init (..)
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

import Prelude hiding (init)

data Form
  = Lifted
  | Unlifted

data MaybeL form a where
  NothingL :: MaybeL Unlifted a
  JustL :: a -> MaybeL Lifted a

instance Functor (MaybeL Unlifted) where
  fmap _ NothingL = NothingL

instance Functor (MaybeL Lifted) where
  fmap f (JustL a) = JustL (f a)

data MaybeU form a where
  NothingU :: MaybeU Lifted a
  JustU :: a -> MaybeU Unlifted a

instance Functor (MaybeU Lifted) where
  fmap _ NothingU = NothingU

instance Functor (MaybeU Unlifted) where
  fmap f (JustU a) = JustU (f a)

data Module form a = Module (Object form a)

deriving instance Functor (Object form) => Functor (Module form)

instance Pretty (Module form a) => Show (Module form a) where
  show = showDefault

instance Pretty (Object form a) => Pretty (Module form a) where
  pretty (Module object) = pretty object

data Object form a
  = Object
    (MaybeL form Fields)
    (Init form a)
    (MaybeL form (Funs form a))

deriving instance ( Functor (MaybeL form)
                  , Functor (Init form)
                  , Functor (Fun form)
                  ) => Functor (Object form)

instance Show (Object Lifted a) where
  show = showDefault

instance Pretty (Object Lifted a) where
  pretty (Object (JustL _fields) init (JustL funs)) =
    text "object" <+> lbrace <>
    (enclose linebreak linebreak . indent 2 $
     vcat (pretty init:map pretty funs)) <>
    rbrace

instance Pretty (Object Unlifted a) where
  pretty (Object NothingL init NothingL) =
    text "object" <+> lbrace <>
    (enclose linebreak linebreak . indent 2 $
     pretty init) <>
    rbrace

type Fields = [Field]

type Field = Ident

data Init form a
  = Init
    (Insns a)
    Vars
    (MaybeU form (Funs form a))

instance Show (Init Lifted a) where
  show = showDefault

instance Show (Init Unlifted a) where
  show = showDefault

instance Pretty (Init Lifted a) where
  pretty (Init insns vars NothingU) =
    vcat (map prettyVar vars <>
          [prettyGraph insns])

instance Pretty (Init Unlifted a) where
  pretty (Init insns vars (JustU funs)) =
    vcat (map prettyVar vars <>
          map pretty funs <>
          [prettyGraph insns])

instance Functor (Init Lifted) where
  fmap f = go
    where
      go (Init insns vars NothingU) =
        Init (mapGraph' f insns) vars NothingU

instance Functor (Init Unlifted) where
  fmap f = go
    where
      go (Init insns vars (JustU funs)) =
        Init (mapGraph' f insns) vars (JustU (map (fmap f) funs))

type Insns a = Graph (Insn a) O C

type Vars = [Ident]

data Fun lifted a
  = Fun
    Ident
    Params
    (Insns a)
    Vars
    (MaybeU lifted (Funs lifted a))

type Params = [Ident]

type Funs lifted a = [Fun lifted a]

instance Show (Fun Lifted a) where
  show = showDefault

instance Show (Fun Unlifted a) where
  show = showDefault

instance Pretty (Fun Lifted a) where
  pretty (Fun x params insns vars NothingU) =
    text "fn" <+> pretty x <> tupled (map pretty params) <+> lbrace <>
    (enclose linebreak linebreak . indent 2 $
     vcat (map prettyVar vars <>
           [prettyGraph insns])) <>
    rbrace

instance Pretty (Fun Unlifted a) where
  pretty (Fun x params insns vars (JustU funs)) =
    text "fn" <+> pretty x <> tupled (map pretty params) <+> lbrace <>
    (enclose linebreak linebreak . indent 2 $
     vcat (map prettyVar vars <>
           map pretty funs <>
           [prettyGraph insns])) <>
    rbrace

prettyVar :: Ident -> Doc e
prettyVar var = text "var" <+> pretty var <> semi

instance Functor (Fun Lifted) where
  fmap f = go
    where
      go (Fun x params insns vars NothingU) =
        Fun x params (mapGraph' f insns) vars NothingU

instance Functor (Fun Unlifted) where
  fmap f = go
    where
      go (Fun x params insns vars (JustU funs)) =
        Fun x params (mapGraph' f insns) vars $ JustU $ map go funs

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

mapGraph'' :: forall a .
              (Insns a -> Insns a) ->
              Module Unlifted a ->
              Module Unlifted a
mapGraph'' f = mapModule
  where
    mapModule :: Module Unlifted a -> Module Unlifted a
    mapModule (Module object) =
      Module (mapObject object)
    mapObject :: Object Unlifted a -> Object Unlifted a
    mapObject (Object fields init NothingL) =
      Object fields (mapInit init) NothingL
    mapInit :: Init Unlifted a -> Init Unlifted a
    mapInit (Init insns vars (JustU funs)) =
      Init (f insns) vars (JustU (map mapFun funs))
    mapFun :: Fun Unlifted a -> Fun Unlifted a
    mapFun (Fun x params insns vars (JustU funs)) =
      Fun x params (f insns) vars (JustU (map mapFun funs))

newtype WrappedInsn e x a
  = WrapInsn { unwrapInsn :: Insn a e x
             }

instance Functor (WrappedInsn e x) where
  fmap (f :: a -> b) = WrapInsn . go . unwrapInsn
    where
      go :: forall e' x' . Insn a e' x' -> Insn b e' x'
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
  BindE :: ExprIdent -> [ExprIdent] -> Int -> Expr
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
      go (BindE expr exprs _) =
        pretty expr <> text "bind" <> tupled (map pretty exprs)
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
