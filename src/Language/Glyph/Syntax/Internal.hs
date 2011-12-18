{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFunctor
  , MultiParamTypeClasses
  , TypeSynonymInstances #-}
module Language.Glyph.Syntax.Internal
       ( module Language.Glyph.View
       , Stmt (..)
       , StmtView (..)
       , Expr (..)
       , ExprView (..)
       , Name (..)
       , NameView
       , ident
       ) where

import Control.Comonad

import Data.Data
import Data.Text (Text)

import Language.Glyph.Ident
import Language.Glyph.View

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
  = IntE Int
  | DoubleE Double
  | BoolE Bool
  | VoidE
  | NotE (Expr a)
  | VarE Name
  | FunE Ident [Name] [Stmt a]
  | ApplyE (Expr a) [Expr a]
  | AssignE Name (Expr a) deriving (Show, Typeable, Data, Functor)

data Name = Name Ident Text deriving (Show, Typeable, Data)

type NameView = Text

instance View (Stmt a) (StmtView a) where
  view (Stmt _ x) = x
  updateView (Stmt x _) = Stmt x

instance View (Expr a) (ExprView a) where
  view (Expr _ x) = x
  updateView (Expr x _) = Expr x

instance View Name NameView where
  view (Name _ x) = x
  updateView (Name x _) = Name x

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