{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , UndecidableInstances
  , ViewPatterns #-}
module Language.Glyph.InferType
       ( inferType
       ) where

import Control.Comonad
import Control.Monad.Reader
import Control.Monad.Writer

import Language.Glyph.Annotation
import Language.Glyph.Annotation.Location
import Language.Glyph.Annotation.Type
import Language.Glyph.HM.InferType hiding (inferType)
import qualified Language.Glyph.HM.InferType as HM
import Language.Glyph.HM.Syntax hiding (Exp (..), ExpView (..), Pat (..))
import qualified Language.Glyph.HM.Syntax as HM
import Language.Glyph.Ident.Internal
import Language.Glyph.IdentMap (IdentMap)
import Language.Glyph.Message
import Language.Glyph.Syntax.Internal

inferType :: ( HasLocation a
            , MonadIdentSupply m
            , MonadWriter Message m
            ) => ([Stmt a], IdentMap b) -> m (Substitution, Type)
inferType (stmts, symtab) = HM.inferType =<< (toExp . transform) stmts

transform :: [Stmt a] -> [Stmt a]
transform = id

toExp :: MonadIdentSupply m => [Stmt a] -> m (HM.Exp a)
toExp stmts = runReaderT' undefined $ runCont $ callCC $ do
  cc <- newIdent
  runWithIdentT' cc $ absE (varP cc) (stmtsToExp stmts)

stmtsToExp :: ( MonadIdentSupply m
             , MonadReader a m
             ) => [Stmt a] -> WithIdentT m (HM.Exp a)
stmtsToExp [] =
  return' undefined'
stmtsToExp (s:ss) =
  local' (extract s) $
  case view s of
    ExprS expr ->
      return' (exprToExp expr)
      `then'`
      stmtsToExp ss
    VarDeclS (ident -> x) expr ->
      let e = maybe undefined' exprToExp expr
      in appE (absE
               (varP x)
               (return' (varE x `asTypeOf'` e) `then'` stmtsToExp ss)) e
    FunDeclS (ident -> f) (map ident -> x) stmts ->
      letE
      [f]
      (fix'
       (absE (tupleP [f]) $
        tupleE [absE (tupleP x) $
          runCont $ callCC $ do
            cc <- newIdent
            absE (varP cc) (withIdent cc (stmtsToExp stmts))]))
      (stmtsToExp ss)
    ReturnS expr -> do
      cc <- askIdent
      appE (varE cc) (maybe voidE exprToExp expr)
      `then'`
      stmtsToExp ss
    IfThenElseS expr stmt Nothing ->
      return' (exprToExp expr `asTypeOf'` boolE True)
      `then'`
      stmtsToExp [stmt]
      `then'`
      stmtsToExp ss
    IfThenElseS expr stmt1 (Just stmt2) ->
      return' (exprToExp expr `asTypeOf'` boolE True)
      `then'`
      stmtsToExp [stmt1]
      `then'`
      stmtsToExp [stmt2]
      `then'`
      stmtsToExp ss
    WhileS expr stmt ->
      return' (exprToExp expr `asTypeOf'` boolE True)
      `then'`
      stmtsToExp [stmt]
      `then'`
      stmtsToExp ss
    BreakS ->
      stmtsToExp ss
    ContinueS ->
      stmtsToExp ss
    ThrowS expr ->
      return' (exprToExp expr)
      `then'`
      stmtsToExp ss
    TryFinallyS stmt1 Nothing ->
      stmtsToExp [stmt1]
      `then'`
      stmtsToExp ss
    TryFinallyS stmt1 (Just stmt2) ->
      stmtsToExp [stmt1]
      `then'`
      stmtsToExp [stmt2]
      `then'`
      stmtsToExp ss
    BlockS stmts ->
      stmtsToExp stmts
      `then'`
      stmtsToExp ss

exprToExp :: ( MonadIdentSupply m
            , MonadReader a m
            ) => Expr a -> WithIdentT m (HM.Exp a)
exprToExp e =
  local' (extract e) $
  case view e of
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
          absE (varP cc) (withIdent cc (stmtsToExp stmts))
    ApplyE expr exprs ->
      appE (exprToExp expr) (tupleE (map exprToExp exprs))
    AssignE (ident -> x) expr ->
      varE x `asTypeOf'` exprToExp expr

runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT

local' :: MonadReader r m => r -> m a -> m a
local' = local . const

runWithIdentT :: WithIdentT m a -> Ident -> m a
runWithIdentT (WithIdentT m) = runReaderT m

runWithIdentT' :: Ident -> WithIdentT m a -> m a
runWithIdentT' = flip runWithIdentT

newtype WithIdentT m a
  = WithIdentT { unWithIdentT :: ReaderT Ident m a
                  } deriving ( Monad
                             , MonadTrans
                             )

instance MonadReader r m => MonadReader r (WithIdentT m) where
  ask = lift ask
  local f (WithIdentT m) = WithIdentT $ (mapReaderT . local) f m

deriving instance MonadWriter w m => MonadWriter w (WithIdentT m)

instance MonadIdentSupply m => MonadIdentSupply (WithIdentT m) where
  newIdent = lift newIdent

askIdent :: Monad m => WithIdentT m Ident
askIdent = WithIdentT ask

withIdent :: Monad m => Ident -> WithIdentT m a -> WithIdentT m a
withIdent x = WithIdentT . local (const x) . unWithIdentT