{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards #-}
module Language.Glyph.Typer
       {-( infer
       )-} where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State

import Data.Generics
import qualified Data.IntMap as TypeVarMap
import qualified Data.IntMap as VarMap
import Data.Maybe

import Language.Glyph.Constraint hiding (True)
import qualified Language.Glyph.Constraint as Constraint
import Language.Glyph.Syntax
import Language.Glyph.Type hiding (Bool, Int, Var)
import qualified Language.Glyph.Type as Type

type Substitution = TypeVarMap Type
type Environment = VarMap TypeScheme

type Var = Int
type Stmt' = Stmt Var
type Expr' = Expr Var

type TypeVarMap = TypeVarMap.IntMap
type VarMap = VarMap.IntMap

newtype Typer a
  = Typer { unTyper :: State S a
          } deriving ( Functor
                     , Applicative
                     , Monad
                     )

data S
  = S { typeVarCount :: Type.Var
      , returnType :: Type
      , explicitReturn :: Bool
      , substitution :: Substitution
      , constraint :: Constraint
      , environment :: Environment
      }

apply :: Data a => TypeVarMap Type -> a -> a
apply phi = everywhere (mkT t)
  where
    t tau@(Type.Var alpha) = fromMaybe tau (TypeVarMap.lookup alpha phi)
    t x = x

typeVars :: Data a => a -> [Type.Var]
typeVars = everything (++) ([] `mkQ` q)
  where
    q (Type.Var x) = [x]
    q _ = []

runTyper :: Typer a -> a
runTyper (Typer m) = evalState m initState

initState :: S
initState =
  S { typeVarCount = 1
    , returnType = Type.Var 0
    , explicitReturn = False
    , substitution = TypeVarMap.empty
    , constraint = Constraint.True
    , environment = VarMap.empty
    }

infer :: Comonad w => [Stmt' w] -> Typer Type
infer ss = do
  alpha <- newType
  withReturnType alpha $ do
    inferStmts ss
    x <- hasExplicitReturn
    when (not x) $ addConstraint $ alpha :==: Void
  return alpha

inferStmts :: Comonad w => [Stmt' w] -> Typer ()
inferStmts = mapM_ inferStmt

inferStmt :: Comonad w => Stmt' w -> Typer ()
inferStmt x =
  case x of
    Expr x' -> do
      inferExpr x'
      return ()

inferExpr :: Comonad w => Expr' w -> Typer Type
inferExpr x =
  case x of
    Int _ ->
      return Type.Int
    
    Bool _ ->
      return Type.Bool
    
    Var x ->
      inferVar x
    
    Fun xs ss ->
      inferFun (map extract xs) (map extract ss)
    
    Apply e es -> do
      alpha <- newType
      tau1 <- inferExpr . extract $ e
      tau2 <- mapM (inferExpr . extract) es
      addConstraint $ tau1 :==: tau2 :->: alpha
      phi <- getSubstitution
      return $ apply phi alpha

inferVar :: Var -> Typer Type
inferVar x = do
  sigma <- lookupTypeScheme x
  (c, tau) <- newInstance sigma
  addConstraint c
  return tau

inferFun :: Comonad w => [Var] -> [Stmt' w] -> Typer Type
inferFun xs ss = do
  alphas <- mapM (const newTypeVar) xs
  tau <- infer ss
  phi <- getSubstitution
  putSubstitution $ phi \\ alphas
  let alphas' = map Type.Var alphas
  return $ apply phi alphas' :->: tau
  where
    phi \\ alphas = TypeVarMap.difference phi alpha
      where
        alpha = TypeVarMap.fromList $ zip alphas (repeat undefined)

newType :: Typer Type
newType = Type.Var <$> newTypeVar

newTypeVar :: Typer Type.Var
newTypeVar = do
  s@S {..} <- get
  let typeVarCount' = typeVarCount + 1
  put s { typeVarCount = typeVarCount' }
  return typeVarCount

withReturnType :: Type -> Typer a -> Typer a
withReturnType returnType m =
  withState (\ s -> s { returnType }) m

getReturnType :: Typer Type
getReturnType = gets returnType

hasExplicitReturn :: Typer Bool
hasExplicitReturn = gets explicitReturn

lookupTypeScheme :: Var -> Typer TypeScheme
lookupTypeScheme x = do
  S {..} <- get
  case VarMap.lookup x environment of
    Just sigma ->
      return sigma
    Nothing ->
      error "lookupTypeScheme"

newInstance :: TypeScheme -> Typer (Constraint, Type)
newInstance sigma = doInst sigma Constraint.True
  where
    doInst (Forall alpha c sigma) d = do
      beta <- newType
      doInst (apply (beta // alpha) sigma) (apply (beta // alpha) c :&&: d)
    doInst (Type tau) c = return (c, tau)
    beta // alpha = TypeVarMap.singleton alpha beta

addConstraint :: Constraint -> Typer ()
addConstraint x = do
  s@S {..} <- get
  let constraint' = constraint :&&: x
  put s { constraint = constraint' }

getSubstitution :: Typer Substitution
getSubstitution = gets substitution

putSubstitution :: Substitution -> Typer ()
putSubstitution substitution = modify $ \ s -> s { substitution }

get :: Typer S
get = Typer State.get

gets :: (S -> a) -> Typer a
gets = Typer . State.gets

put :: S -> Typer ()
put = Typer . State.put

modify :: (S -> S) -> Typer ()
modify = Typer . State.modify

withState :: (S -> S) -> Typer a -> Typer a
withState f (Typer m) = Typer $ State.withState f m