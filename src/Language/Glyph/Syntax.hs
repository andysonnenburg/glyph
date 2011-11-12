{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , FlexibleContexts
  , StandaloneDeriving #-}
module Language.Glyph.Syntax
       ( Stmt (..)
       , Expr (..)
       , Named (..)
       , Name
       , name
       ) where

import Control.Comonad

import Data.Data
import Data.Foldable
import Data.Text (Text)
import Data.Traversable

import Language.Glyph.Location

data Stmt a
  = ExprS (Located (Expr a))
  | VarDeclS (Located (Named a)) (Maybe (Located (Expr a)))
  | FunDeclS (Located (Named a)) [Located (Named a)] [Located (Stmt a)]
  | ReturnS (Maybe (Located (Expr a)))
  | IfThenElseS (Located (Expr a)) (Located (Stmt a)) (Maybe (Located (Stmt a)))
  | WhileS (Located (Expr a)) (Located (Stmt a))
  | BlockS [Located (Stmt a)] deriving ( Show
                                       , Typeable
                                       , Data
                                       , Functor
                                       , Foldable
                                       , Traversable
                                       )

data Expr a
  = IntE Int
  | BoolE Bool
  | VarE (Named a)
  | FunE a [Located (Named a)] [Located (Stmt a)]
  | ObjE [(Located Text, Located (Expr a))]
  | ApplyE (Located (Expr a)) [Located (Expr a)]
  | AssignE (Located (Named a)) (Located (Expr a)) deriving ( Show
                                                          , Typeable
                                                          , Data
                                                          , Functor
                                                          , Foldable
                                                          , Traversable
                                                          )

data Named a
  = Named Name a deriving ( Show
                          , Functor
                          , Foldable
                          , Traversable
                          , Typeable
                          , Data
                          )

type Name = Text

name :: Named a -> Name
name (Named x _) = x

instance Extend Named where
  duplicate w@(Named x _) = Named x w
  extend f w@(Named x _) = Named x (f w)

instance Comonad Named where
  extract (Named _ a) = a
