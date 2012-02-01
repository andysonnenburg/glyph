{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFunctor
  , FlexibleInstances
  , MultiParamTypeClasses
  , TypeSynonymInstances #-}
module Language.Glyph.Syntax.Internal
       ( module X
       , Stmt (..)
       , StmtView (..)
       , Expr (..)
       , ExprView (..)
       , Lit (..)
       , Name (..)
       , NameView
       , ident
       ) where

import Control.Comonad

import Data.Data
import Data.Int
import Data.Text (Text)
import qualified Data.Text as Text

import Language.Glyph.Ident
import Language.Glyph.Update as X
import Language.Glyph.View as X

import Text.PrettyPrint.Free

data Stmt a = Stmt a (StmtView a) deriving (Show, Typeable, Data, Functor)

data StmtView a
  = ExprS (Expr a)
  | VarDeclS Name (Maybe (Expr a))
  | FunDeclS Name [Name] [Stmt a]
  | ReturnS (Maybe (Expr a))
  | IfThenElseS (Expr a) (Stmt a) (Maybe (Stmt a))
  | WhileS (Expr a) (Stmt a)
  | BreakS
  | ContinueS
  | ThrowS (Expr a)
  | TryFinallyS (Stmt a) (Maybe (Stmt a))
  | BlockS [Stmt a] deriving (Show, Typeable, Data, Functor)

data Expr a = Expr a (ExprView a) deriving (Show, Typeable, Data, Functor)

data ExprView a
  = LitE Lit
  | NotE (Expr a)
  | VarE Name
  | FunE Ident [Name] [Stmt a]
  | ApplyE (Expr a) [Expr a]
  | AssignE Name (Expr a) deriving (Show, Typeable, Data, Functor)

data Lit
  = IntL Int32
  | DoubleL Double
  | BoolL Bool
  | VoidL deriving (Eq, Typeable, Data)

instance Pretty Lit where
  pretty (IntL x) = pretty . toInteger $ x
  pretty (DoubleL x) = pretty x
  pretty (BoolL True) = text "true"
  pretty (BoolL False) = text "false"
  pretty VoidL = text "void"

instance Show Lit where
  show = show . pretty

data Name = Name Ident NameView deriving (Show, Typeable, Data)

instance Pretty Name where
  pretty = pretty . text . Text.unpack . view

type NameView = Text

instance View (Stmt a) (StmtView a) where
  view (Stmt _ x) = x

instance Update (Stmt a) (StmtView a) where
  update (Stmt a _) = Stmt a

instance View (Expr a) (ExprView a) where
  view (Expr _ x) = x

instance Update (Expr a) (ExprView a) where
  update (Expr a _) = Expr a

instance View Name NameView where
  view (Name _ x) = x

instance Update Name NameView where
  update (Name a _) = Name a

instance Extend Stmt where
  duplicate w@(Stmt _ _) = Stmt w undefined
  extend f w@(Stmt _ _) = Stmt (f w) undefined

instance Extend Expr where
  duplicate w@(Expr _ _) = Expr w undefined
  extend f w@(Expr _ _) = Expr (f w) undefined

instance Comonad Stmt where
  extract (Stmt x _) = x

instance Comonad Expr where
  extract (Expr x _) = x

ident :: Name -> Ident
ident (Name x _) = x
