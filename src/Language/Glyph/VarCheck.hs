{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , UndecidableInstances #-}
module Language.Glyph.VarCheck
       ( varCheck
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.State

import Data.Typeable

import Language.Glyph.Location
import Language.Glyph.Syntax

varCheck :: MonadError (VarCheckException var) m =>
           [Located (Stmt Located var)] ->
           m [Located (Stmt Located var)]
varCheck ss = undefined
  where
    funDecls = funDeclsQ
    callGraph = callGraphQ ss
    sccs = map flattenSCC . stronglyConnCompR $ callGraph
    freeVars = map (Set.difference . Set.unions . map freeVarsQ) sccs
    callSets = map (Set.unions . map callSetQ) sccs
    
    funDeclsQ = everything (++) ([] `mkQ` f)
      where
        f (FunDecl x _ _) = [extract x]
        f _ = []
    
    callGraphQ = everything (++) ([] `mk` f)
      where
        f x@(FunDecl a ps ss) = do
          let funs = varsQ ss `intersection` funDecls
              nested = funDeclsQ ss
              callSet = funs `difference` nested
              freeVars = varDeclsQ
          return ()

freeVars :: [Stmt' var] -> [(var, [var])]

callSets :: [Stmt' var] -> [(var, [var])]

extraSets :: [Stmt' var] -> Map var (Set var)
extraSets ss = ess
  where
    dvs = declVars ss
    
    fvs = freeVars' ss
    
    css = Map.fromList . callSets $ ss
    
    xss = map flattenSCC .
          stronglyConnComp .
          map ((x, xs) -> (x, x, xs)) $ css'
    
    ess = Map.fromList . concatMap f $ xss
      where
        f = undefined

newtype VarCheckerT m a
  = VarCheckerT { unVarCheckerT :: StateT S m a
                } deriving ( Functor
                           , Applicative
                           , Monad
                           )

deriving instance MonadError (VarCheckException var) m => MonadError (VarCheckException var) (VarCheckerT m)

data S var
  = S { initialized :: Set var
      , extraSets :: Map var [var]
      }

data VarCheckException var
  = NotInitizialized (Located var)
  | StrMsgError String
  | NoMsgError deriving (Show, Typeable)

instance (Show var, Typeable var) => Exception (VarCheckException var)

instance Error (VarCheckException var) where
  strMsg = StrMsgError
  noMsg = NoMsgError

runVarCheckerT :: Monad m => VarCheckerT m a -> m a
runVarCheckerT (VarCheckerT m) = undefined

type Stmt' var = Located (Stmt Located var)
type Expr' var = Located (Expr Located var)

varCheckStmts :: MonadError VarCheckException m =>
                [Stmt' var] ->
                VarCheckerT m ()
varCheckStmts ss = mapM_ varCheckStmt ss

varCheckStmt :: MonadError VarCheckException m =>
               Stmt' var ->
               VarCheckerT m ()
varCheckStmt x =
  case x of
    Expr e ->
      varCheckExpr e
    
    VarDecl v m ->
      mapM_ varCheckExpr m
      checkInitialized v
    
    FunDecl v ps ss -> do
      mapM_ setInitialized ps
      varCheckStmts ss
    
    Return m ->
      mapM_ varCheckExpr m
    
    IfThenElse e s m -> do
      varCheckExpr e
      s@S {..} <- get
      varCheckStmt s
      S <- get
      putInitalized initialized
      
      put s { initialized = Map.union
            }
      
      
    
    


