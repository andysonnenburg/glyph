{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
module Language.Glyph.InferType
       ( inferType
       ) where

import Control.Monad.Reader
import Control.Monad.Writer

import Language.Glyph.Annotation
import Language.Glyph.Annotation.Type
import Language.Glyph.HM.InferType hiding (inferType)
import qualified Language.Glyph.HM.InferType as HM
import Language.Glyph.HM.Syntax hiding (Exp (..), Pat (..))
import qualified Language.Glyph.HM.Syntax as HM
import Language.Glyph.Ident.Internal
import Language.Glyph.IdentMap (IdentMap)
import Language.Glyph.Message
import Language.Glyph.Syntax.Internal

inferType :: ( MonadIdentSupply m
            , MonadWriter Message m
            ) => ([Stmt a], IdentMap b) -> m (Substitution, Type)
inferType (stmts, symtab) = HM.inferType =<< (toExp . transform) stmts

transform :: [Stmt a] -> [Stmt a]
transform = id

toExp :: MonadIdentSupply m => [Stmt a] -> m HM.Exp
toExp stmts = runCont $ callCC $ do
  cc <- newIdent
  absE (varP cc) (runReaderT (stmtsToExp stmts) cc)

stmtsToExp :: (MonadIdentSupply m, MonadReader Ident m) => [Stmt a] -> m HM.Exp
stmtsToExp [] =
  return' undefined'
stmtsToExp ((view -> v):xs) =
  case v of
    ExprS expr ->
      return' (exprToExp expr)
      `then'`
      stmtsToExp xs
    VarDeclS (ident -> x) expr ->
      let e = maybe undefined' exprToExp expr
      in appE (absE
               (varP x)
               (return' (varE x `asTypeOf'` e) `then'` stmtsToExp xs)) e
    FunDeclS (ident -> f) (map ident -> x) stmts ->
      letE
      [f]
      (fix'
       (absE (tupleP [f]) $
        tupleE [absE (tupleP x) $
          runCont $ callCC $ do
            cc <- newIdent
            absE (varP cc) (local (const cc) (stmtsToExp stmts))]))
      (stmtsToExp xs)
    ReturnS expr -> do
      cc <- ask
      appE (varE cc) (maybe voidE exprToExp expr)
      `then'`
      stmtsToExp xs
    IfThenElseS expr stmt Nothing ->
      return' (exprToExp expr `asTypeOf'` boolE True)
      `then'`
      stmtsToExp [stmt]
      `then'`
      stmtsToExp xs
    IfThenElseS expr stmt1 (Just stmt2) ->
      return' (exprToExp expr `asTypeOf'` boolE True)
      `then'`
      stmtsToExp [stmt1]
      `then'`
      stmtsToExp [stmt2]
      `then'`
      stmtsToExp xs
    WhileS expr stmt ->
      return' (exprToExp expr `asTypeOf'` boolE True)
      `then'`
      stmtsToExp [stmt]
      `then'`
      stmtsToExp xs
    BreakS ->
      stmtsToExp xs
    ContinueS ->
      stmtsToExp xs
    ThrowS expr ->
      return' (exprToExp expr)
      `then'`
      stmtsToExp xs
    TryFinallyS stmt1 Nothing ->
      stmtsToExp [stmt1]
      `then'`
      stmtsToExp xs
    TryFinallyS stmt1 (Just stmt2) ->
      stmtsToExp [stmt1]
      `then'`
      stmtsToExp [stmt2]
      `then'`
      stmtsToExp xs
    BlockS stmts ->
      stmtsToExp stmts
      `then'`
      stmtsToExp xs

exprToExp :: (MonadIdentSupply m, MonadReader Ident m) => Expr a -> m HM.Exp
exprToExp (view -> v) =
  case v of
    IntE int ->
      intE int
    DoubleE double ->
      doubleE double
    BoolE bool ->
      boolE bool
    VoidE ->
      voidE
    NotE expr ->
      exprToExp expr `asTypeOf'` boolE True
    VarE (ident -> x) ->
      varE x
    FunE _ (map ident -> x) stmts ->
      absE (tupleP x) $
        runCont $ callCC $ do
          cc <- newIdent
          absE (varP cc) (local (const cc) (stmtsToExp stmts))
    ApplyE expr exprs ->
      appE (exprToExp expr) (tupleE (map exprToExp exprs))
    AssignE (ident -> x) expr ->
      varE x `asTypeOf'` exprToExp expr
