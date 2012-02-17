{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , StandaloneDeriving
  , UndecidableInstances
  , ViewPatterns #-}
module Language.Glyph.InferType
       ( inferType
       ) where

import Control.Applicative
import Control.Comonad
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Graph hiding (scc)

import Language.Glyph.Annotation
import Language.Glyph.Annotation.CallSet
import Language.Glyph.Annotation.Location
import Language.Glyph.Annotation.Type
import Language.Glyph.Generics
import Language.Glyph.HM.InferType hiding (inferType)
import qualified Language.Glyph.HM.InferType as HM
import Language.Glyph.HM.Syntax hiding (Exp (..), ExpView (..), Pat (..))
import qualified Language.Glyph.HM.Syntax as HM
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Logger
import Language.Glyph.Message
import Language.Glyph.Symtab
import Language.Glyph.Syntax
import Language.Glyph.Unique

inferType :: ( Data a
            , HasLocation a
            , Monoid a
            , HasCallSet b
            , MonadLogger Message m
            , UniqueMonad m
            ) => ([Stmt a], IdentMap b) -> m (Substitution, Type)
inferType (stmts, symtab) = HM.inferType =<< runSymtabT (toExp stmts) symtab

toExp :: ( Data a
        , Monoid a
        , HasCallSet b
        , MonadSymtab b m
        , UniqueMonad m
        ) => [Stmt a] -> m (HM.Exp a)
toExp stmts =
  runReaderT' (mconcat' $ map extract stmts) $
  runCont $ callCC $ do
    cc <- freshIdent
    runWithIdentT' cc $ absE (varP cc) (blockToExp stmts)

mconcat' :: Monoid a => [a] -> a
mconcat' = go
  where
    go [] = mempty
    go (x : xs) = foldl (<>) x xs

stmtsToExp :: ( Data a
             , Monoid a
             , HasCallSet b
             , MonadReader a m
             , MonadSymtab b m
             , UniqueMonad m
             ) => [Stmt a] -> WithIdentT m (HM.Exp a)
stmtsToExp [] =
  return' undefined'
stmtsToExp (s : ss) =
  local' (extract s) $
  case view s of
    ExprS expr ->
      return' (exprToExp expr)
      `then'`
      stmtsToExp ss
    VarDeclS (ident -> x) expr ->
      let e = maybe undefined' exprToExp expr
      in return' (varE x `asTypeOf'` e)
         `then'`
         stmtsToExp ss
    FunDeclS _name _params _stmts ->
      stmtsToExp ss
    ReturnS expr -> do
      cc <- askIdent
      appE (varE cc) (maybe (litE VoidL) exprToExp expr)
      `then'`
      stmtsToExp ss
    IfThenElseS expr stmt Nothing ->
      return' (exprToExp expr `asTypeOf'` litE (BoolL True))
      `then'`
      stmtsToExp [stmt]
      `then'`
      stmtsToExp ss
    IfThenElseS expr stmt1 (Just stmt2) ->
      return' (exprToExp expr `asTypeOf'` litE (BoolL True))
      `then'`
      stmtsToExp [stmt1]
      `then'`
      stmtsToExp [stmt2]
      `then'`
      stmtsToExp ss
    WhileS expr stmt ->
      return' (exprToExp expr `asTypeOf'` litE (BoolL True))
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
      blockToExp stmts
      `then'`
      stmtsToExp ss

exprToExp :: ( Data a
            , HasCallSet b
            , MonadReader a m
            , MonadSymtab b m
            , UniqueMonad m
            ) => Expr a -> WithIdentT m (HM.Exp a)
exprToExp e =
  local' (extract e) $
  case view e of
    LitE lit ->
      litE lit
    NotE expr ->
      exprToExp expr `asTypeOf'` litE (BoolL True)
    VarE (ident -> x) ->
      varE x
    FunE x _params _stmts ->
      varE x
    ApplyE expr exprs ->
      appE (exprToExp expr) (tupleE (map exprToExp exprs))
    AssignE (ident -> x) expr ->
      varE x `asTypeOf'` exprToExp expr

funToExp :: ( Data a
           , Monoid a
           , HasCallSet b
           , MonadReader a m
           , MonadSymtab b m
           , UniqueMonad m
           ) => [Name] -> [Stmt a] -> m (HM.Exp a)
funToExp (map ident -> x) stmts =
  absE (tupleP x) $
    runCont $ callCC $ do
      cc <- freshIdent
      absE (varP cc) (runWithIdentT' cc (blockToExp stmts))

blockToExp :: ( Data a
             , Monoid a
             , HasCallSet b
             , MonadReader a m
             , MonadSymtab b m
             , UniqueMonad m
             ) => [Stmt a] -> WithIdentT m (HM.Exp a)
blockToExp stmts =
  local' (mconcat' (map extract stmts)) .
  varDecls . funs . stmtsToExp $ stmts
  where
    varDecls e2 =
      everythingThisScope (.) (id `mkQ` queryStmt) stmts e2
      where
        queryStmt stmt@(view -> VarDeclS (ident -> x) _expr) =
          local' (extract stmt) . f
          where
            f e' = appE (absE (tupleP [x]) e') (tupleE [e])
            e = undefined'
        queryStmt _ = id

    funs e2 = do
      callGraph <- callGraphM stmts
      let scc = stronglyConnCompR callGraph
          lets = map (((,) <$> map snd3 <*> map fst3) . flattenSCC) scc
      foldr f e2 lets
      where
        f (x, e1) = letE (tupleP x) (fix' (absE (tupleP x) (tupleE e1)))
        fst3 (x, _, _) = x
        snd3 (_, x, _) = x

    callGraphM :: forall a b m .
                 ( Data a
                 , Monoid a
                 , HasCallSet b
                 , MonadReader a m
                 , MonadSymtab b m
                 , UniqueMonad m
                 ) => [Stmt a] -> m [(m (HM.Exp a), Ident, [Ident])]
    callGraphM =
      everythingThisScope append $
      return mempty `mkQ` queryStmt `extQ` queryExpr
      where
        append = liftM2 (<>)

        queryStmt :: Stmt a -> m [(m (HM.Exp a), Ident, [Ident])]
        queryStmt stmt'@(view -> FunDeclS (ident -> x) params stmts') = do
          symtab <- askSymtab
          return [(local' r e, x, IdentSet.toList . callSet $ symtab !x)]
          where
            r = extract stmt'
            e = funToExp params stmts'
        queryStmt _ =
          return mempty

        queryExpr :: Expr a -> m [(m (HM.Exp a), Ident, [Ident])]
        queryExpr expr@(view -> FunE x params stmts') = do
          symtab <- askSymtab
          return [(local' r e, x, IdentSet.toList . callSet $ symtab !x)]
          where
            r = extract expr
            e = funToExp params stmts'
        queryExpr _ =
          return mempty

runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT

local' :: MonadReader r m => r -> m a -> m a
local' = local . const

runWithIdentT :: WithIdentT m a -> Ident -> m a
runWithIdentT (WithIdentT m) = runReaderT m

runWithIdentT' :: Ident -> WithIdentT m a -> m a
runWithIdentT' = flip runWithIdentT

newtype WithIdentT m a
  = WithIdentT ( ReaderT Ident m a
               ) deriving ( Monad
                          , MonadTrans
                          )

instance MonadReader r m => MonadReader r (WithIdentT m) where
  ask = lift ask
  local f (WithIdentT m) = WithIdentT $ (mapReaderT . local) f m

deriving instance MonadWriter w m => MonadWriter w (WithIdentT m)

instance UniqueMonad m => UniqueMonad (WithIdentT m) where
  freshUnique = lift freshUnique

instance MonadSymtab r m => MonadSymtab r (WithIdentT m) where
  askSymtab = lift askSymtab

askIdent :: Monad m => WithIdentT m Ident
askIdent = WithIdentT ask
