{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards #-}
module Language.Glyph.Internal.Typer
       ( inferType
       ) where

import Control.Applicative
import Control.Monad.State hiding (get, modify, put)
import qualified Control.Monad.State as State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Glyph.Internal.Exp (Exp)
import qualified Language.Glyph.Internal.Exp as Exp
import Language.Glyph.Internal.Type (Type ((:->:)))
import qualified Language.Glyph.Internal.Type as Type

import Prelude hiding (abs)

import Debug.Trace

inferType e = runTyper $ do
  (a, c, tau) <- bu e
  solve c

runTyper :: Typer a -> a
runTyper = flip evalState initState . unTyper
  where
    initState = S 0 Set.empty

get :: Typer S
get = Typer State.get

put :: S -> Typer ()
put = Typer . State.put

newtype Typer a
  = Typer { unTyper :: State S a
          } deriving ( Functor
                     , Applicative
                     , Monad
                     )

data S
  = S { typeVarCount :: Type.Var
      , monomorphicTypeVars :: Set Type.Var
      }

type Assumption = (Type.Var, Type)

type Assumptions = [Assumption]

data Constraint
  = Type :==: Type

instance Show Constraint where
  show (tau1 :==: tau2) = show tau1 ++ " == " ++ show tau2

infixl 8 :==:

type Constraints = [Constraint]

type Rule = Typer (Assumptions, Constraints, Type)

type Subst = Map Type.Var Type

newTypeVar :: Typer Type.Var
newTypeVar = do
  s@S {..} <- get
  let x = typeVarCount
  put s { typeVarCount = typeVarCount + 1 }
  return x

withMonomorphicTypeVar :: Type.Var -> Typer a -> Typer a
withMonomorphicTypeVar beta m = do
  s@S {..} <- get
  put s { monomorphicTypeVars = Set.insert beta monomorphicTypeVars }
  a <- m
  put s { monomorphicTypeVars }
  return a

getMonomorphicTypeVars :: Typer (Set Type.Var)
getMonomorphicTypeVars = do
  S {..} <- get
  return monomorphicTypeVars

solve :: Constraints -> Typer Subst
solve [] = return nullSubst
solve (tau1 :==: tau2:c) = do
  s <- mgu tau1 tau2
  s' <- solve (apply s c)
  return (s' `compose` s)

mgu :: Type -> Type -> Typer Subst
mgu (l :->: r) (l' :->: r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
mgu (Type.Var u) t = varBind u t
mgu t (Type.Var u) = varBind u t
mgu t1 t2
  | t1 == t2 = return Map.empty
mgu t1 t2 = fail "types do not unify"

varBind :: Type.Var -> Type -> Typer Subst
varBind u t | t == Type.Var u = return nullSubst
            | otherwise = return (Map.singleton u t)

nullSubst :: Subst
nullSubst = Map.empty

class Apply a where
  apply :: Subst -> a -> a

instance Apply Type
instance Apply a => Apply [a]
instance Apply Constraint

compose :: Subst -> Subst -> Subst
compose = undefined

bu :: Exp -> Rule
bu (Exp.Var x) = var x
bu (Exp.Int x) = int x
bu (Exp.Bool x) = bool x
bu (Exp.App e1 e2) = app e1 e2
bu (Exp.Abs x e) = abs x e
bu (Exp.Let x e1 e2) = let' x e1 e2

var :: Exp.Var -> Rule
var x = do
  beta <- Type.Var <$> newTypeVar
  return ([(x, beta)], [], beta)

int :: Int -> Rule
int _ = return ([], [], Type.Int)

bool :: Bool -> Rule
bool _ = return ([], [], Type.Bool)

app :: Exp -> Exp -> Rule
app e1 e2 = do
  (a1, c1, tau1) <- bu e1
  (a2, c2, tau2) <- bu e2
  beta <- Type.Var <$> newTypeVar
  return (a1 `u` a2, c1 `u` c2 `u` [tau1 :==: tau2 :->: beta], beta)

abs :: Exp.Var -> Exp -> Rule
abs x e = do
  beta <- newTypeVar
  let beta' = Type.Var beta
  withMonomorphicTypeVar beta $ do
    (a, c, tau) <- bu e
    return (a \\ x,
            c `u` [tau' :==: beta' | (x', tau') <- a, x == x'],
            beta' :->: tau)

let' :: Exp.Var -> Exp -> Exp -> Rule
let' x e1 e2 = do
  (a1, c1, tau1) <- bu e1
  (a2, c2, tau2) <- bu e2
  m <- getMonomorphicTypeVars
  return (a1 `u` a2 \\ x,
          c1 `u` c2 `u` [tau' :==: tau1 | (x', tau') <- a2, x == x'],
          tau2)

u :: [a] -> [a] -> [a]
u = (++)

(\\) :: Assumptions -> Type.Var -> Assumptions
xs \\ x = filter ((/= x) . fst) xs