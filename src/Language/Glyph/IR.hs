{-# LANGUAGE
    ExistentialQuantification
  , GADTs
  , ViewPatterns
  , MultiParamTypeClasses #-}
module Language.Glyph.IR
       ( Stmt (..)
       , StmtView (..)
       , MaybeExpr (..)
       , Expr (..)
       , ExprView (..)
       , SomeExpr (..)
       , Lit (..)
       ) where

import Compiler.Hoopl

import Data.Int
import Data.Maybe

import Language.Glyph.Ident
import Language.Glyph.Syntax.Internal (Name)
import qualified Language.Glyph.Syntax.Internal as Syntax
import Language.Glyph.View

data Stmts a = Stmts Label (Graph (Stmt a) C C)

data Stmt a e x where
  Stmt :: a -> StmtView a O x -> Stmt a O x
  LabelS :: Label -> Stmt a C O
  ReturnVoidS :: Stmt a O C

data StmtView a e x where
  ExprS :: Expr a x -> StmtView a O x
  VarDeclS :: Name -> MaybeExpr a x -> StmtView a O x
  FunDeclS :: Name -> [Name] -> Graph (Stmt a) C C -> StmtView a O O
  ReturnS :: MaybeExpr a x -> StmtView a O C
  IfS :: ShapedExpr a x -> Label -> Label -> StmtView a O C
  GotoS :: Label -> StmtView a O C
  ThrowS :: ShapedExpr a x -> Maybe Label -> StmtView a O C
  TryS :: StmtView a O O
  EndTryS :: Label -> StmtView a O C
  FinallyS :: Label -> StmtView a O O
  EndFinallyS :: Maybe Label -> Label -> StmtView a O C

data MaybeExpr a x where
  JustExprC :: Expr a C -> MaybeExpr a C
  JustExprO :: Expr a O -> MaybeExpr a O
  NothingExpr :: MaybeExpr a O

data ShapedExpr a x where
  ClosedExpr :: Expr a C -> ShapedExpr a C
  OpenExpr :: Expr a O -> ShapedExpr a O

data Expr a x = Expr a (ExprView a x)

data ExprView a x where
  LitE :: Lit -> ExprView a O
  NotE :: Expr a x -> ExprView a x
  VarE :: Name -> ExprView a O
  FunE :: Ident -> [Name] -> (Label, Graph (Stmt a) C C) -> ExprView a O
  ApplyE :: ShapedExpr a x -> [SomeExpr a] -> Maybe Label -> Label -> ExprView a C
  AssignE :: Name -> Expr a x -> ExprView a x

data SomeExpr a = forall x . SomeExpr (ShapedExpr a x)

data Lit
  = IntL Int32
  | DoubleL Double
  | BoolL Bool
  | VoidL

instance View (Expr a x) (ExprView a x) where
  view (Expr _a x) = x
  updateView (Expr a _) = Expr a

instance NonLocal (Stmt a) where 
  entryLabel (LabelS label) = label
  
  successors ReturnVoidS = []
  successors (Stmt _a x) =
    case x of
      ExprS expr ->
        exprSuccessors expr
      VarDeclS _name (JustExprC expr) -> 
        exprSuccessors expr
      ReturnS (JustExprC expr) ->
        exprSuccessors expr
      ReturnS _ ->
        []
      IfS (ClosedExpr expr) then' else' ->
        exprSuccessors expr ++ [then', else']
      IfS (OpenExpr _expr) then' else' ->
        [then', else']
      GotoS label ->
        [label]
      ThrowS (ClosedExpr expr) (maybeToList -> finally) ->
        exprSuccessors expr ++ finally
      ThrowS (OpenExpr _expr) (maybeToList -> finally) ->
        finally
      EndTryS finally ->
        [finally]
      EndFinallyS (maybeToList -> finally) successor ->
        finally ++ [successor]

exprSuccessors :: Expr a C -> [Label]
exprSuccessors (view -> x) =
  case x of
    NotE expr ->
      exprSuccessors expr
    ApplyE shapedExpr someExprs (maybeToList -> finally) label ->
      shapedExprSuccessors shapedExpr ++
      concatMap someExprSuccessors someExprs ++
      finally ++
      [label]
    AssignE _name expr ->
      exprSuccessors expr

shapedExprSuccessors :: ShapedExpr a x -> [Label]
shapedExprSuccessors x =
  case x of
    ClosedExpr expr -> exprSuccessors expr
    OpenExpr _expr -> []

someExprSuccessors :: SomeExpr a -> [Label]
someExprSuccessors (SomeExpr shapedExpr) = shapedExprSuccessors shapedExpr
