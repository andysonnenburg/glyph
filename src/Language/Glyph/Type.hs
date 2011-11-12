{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving #-}
module Language.Glyph.Type
       ( TypeScheme (..)
       , Type (..)
       , Label
       , Var
       , Typed (..)
       , TypeException (..)
       , type'
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.State

import Data.Typeable

import Language.Glyph.Internal.Type
import Language.Glyph.Location
import Language.Glyph.Name (Name)
import Language.Glyph.Syntax

type' :: MonadError TypeException m =>
        [Located (Stmt Located Name)] ->
        m [Located (Stmt Located (Typed Name))]
type' = runTyperT . typeStmts

newtype TyperT m a
  = TyperT { unTyperT :: StateT S m a
           } deriving ( Functor
                      , Applicative
                      , Monad
                      )

deriving instance MonadError TypeException m => MonadError TypeException (TyperT m)

data S = S { typeVarCount :: Var
           , returnType :: Var
           , explicitReturn :: Bool
           , substitution :: Substitution
           , constraint :: Constraint
           , environment :: Environment
           }

data Typed a = Typed TypeScheme a deriving Show

data TypeException
  = UnifyError (Located Type) (Located Type)
  | StrMsgError String
  | NoMsgError deriving (Show, Typeable)

instance Exception TypeException

instance Error TypeException where
  strMsg = StrMsgError
  noMsg = NoMsgError

runTyperT :: Monad m => TyperT m a -> m a
runTyperT = undefined

type Stmt' var = Located (Stmt Located var)
type Expr' var = Located (Expr Located var)

typeStmts :: MonadError TypeException m =>
            [Stmt' Name] ->
            TyperT m [Stmt' (Typed Name)]
typeStmts = undefined
