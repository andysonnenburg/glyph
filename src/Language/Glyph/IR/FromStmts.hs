{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RankNTypes
  , TypeSynonymInstances #-}
module Language.Glyph.IR.FromStmts
       ( ContFlowException (..)
       , fromStmts
       ) where

import Control.Comonad
import Control.Exception hiding (finally)
import Compiler.Hoopl hiding ((<*>))
import qualified Compiler.Hoopl as Hoopl
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Typeable

import Language.Glyph.IR.RemoveUnreachable
import Language.Glyph.IR.Syntax
import Language.Glyph.Ident
import qualified Language.Glyph.Syntax.Internal as Glyph

import Prelude hiding (last)

fromStmts :: ( Monoid a
           , MonadError ContFlowException m
           , UniqueMonad m
           ) => [Glyph.Stmt a] -> m (Graph (Insn a) O C)
fromStmts = fromFun []

fromFun :: ( Monoid a
           , MonadError ContFlowException m
           , UniqueMonad m
           ) => [Ident] -> [Glyph.Stmt a] -> m (Graph (Insn a) O C)
fromFun params stmts = do
  graph <- runReaderT' r . liftM unW . execWriterT . mapM_ tellStmt $ stmts
  let graph' = graph |<*>| mkLast ReturnVoid
  return $ removeUnreachable graph'
  where
    runReaderT' = flip runReaderT
    
    r = R { annotation = mconcat $ map extract stmts
          , finally = initFinally
          , maybeCatchLabel = Nothing
          , maybeLoopLabels = Nothing
          }

initFinally :: Monad m => Finally m
initFinally = Finally { beforeLoopExit
                      , beforeReturn
                      }
  where
    beforeLoopExit =
      return ()
    beforeReturn =
      return ()

data ContFlowException
  = IllegalBreak
  | IllegalContinue
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show ContFlowException where
  show x =
    case x of
      IllegalBreak -> "illegal break statement"
      IllegalContinue -> "illegal continue statement"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception ContFlowException

tellStmt :: ( MonadError ContFlowException m
            , MonadReader (R a) m
            , MonadWriter (W a) m
            , Monoid a
            , UniqueMonad m
            ) => Glyph.Stmt a -> m ()
tellStmt = tellStmt'
  where
    tellStmt' (Glyph.Stmt a x) =
      localA a $ go x
    go (Glyph.ExprS e) = do
      x <- tellExpr e
      tellS' $ ExprS x
    go (Glyph.VarDeclS name Nothing) =
      tellS $ VarDeclS name
    go (Glyph.VarDeclS name (Just expr)) = do
      tellS $ VarDeclS name
      x <- tellExpr expr >>= tellE . AssignE name
      tellS' $ ExprS x
    go (Glyph.FunDeclS name params stmts) =
      tellS =<< FunDeclS name params <$> fromFun (map ident params) stmts
    go (Glyph.ReturnS Nothing) = do
      x <- tellE $ LitE VoidL
      join $ asks $ beforeReturn . finally
      tellS $ ReturnS x
    go (Glyph.ReturnS (Just expr)) = do
      x <- tellExpr expr
      join $ asks $ beforeReturn . finally
      tellS $ ReturnS x
    go (Glyph.IfThenElseS expr stmt Nothing) = do
      x <- tellExpr expr
      (thenLabel, nextLabel) <- freshLabels
      tellS $ IfS x thenLabel nextLabel
      tellLabel thenLabel
      tellStmt stmt
      tellGoto nextLabel
      tellLabel nextLabel
    go (Glyph.IfThenElseS expr stmt1 (Just stmt2)) = do
      x <- tellExpr expr
      (thenLabel, elseLabel, nextLabel) <- freshLabels
      tellS $ IfS x thenLabel elseLabel
      tellLabel thenLabel
      tellStmt stmt1
      tellGoto nextLabel
      tellLabel elseLabel
      tellStmt stmt2
      tellGoto nextLabel
      tellLabel nextLabel
    go (Glyph.WhileS expr stmt) = do
      testLabel <- freshLabel
      tellLabel testLabel
      x <- tellExpr expr
      (thenLabel, nextLabel) <- freshLabels
      tellS $ IfS x thenLabel nextLabel
      tellLabel thenLabel
      localLoop nextLabel testLabel $ tellStmt stmt
      tellGoto testLabel
      tellLabel nextLabel
    go Glyph.BreakS = do
      join $ asks $ beforeLoopExit . finally
      askBreakLabel >>= tellS . GotoS
    go Glyph.ContinueS = do
      join $ asks $ beforeLoopExit . finally
      askContinueLabel >>= tellS . GotoS
    go (Glyph.ThrowS expr) =
      tellExpr expr >>=
      tellS . ThrowS
    go (Glyph.TryFinallyS stmt Nothing) =
      tellStmt stmt
    go (Glyph.TryFinallyS stmt1 (Just stmt2)) = do
      (nextLabel, catchLabel) <- freshLabels
      last <- localFinally catchLabel stmt2 $ do
        tellStmt stmt1
        getLastS $ GotoS nextLabel
      x <- freshIdent
      let first = mkFirst $ Catch x catchLabel
      tell $ W $ last |*><*| first
      tellStmt stmt2
      tellS $ ThrowS x
      tellLabel nextLabel
      tellStmt stmt2
    go (Glyph.BlockS stmts) =
      mapM_ tellStmt stmts
    
    localFinally catchLabel finallyStmt =
      local f
      where
        f r@R { finally } =
          r { finally =
                 let tellFinally = local (const r) $ tellStmt finallyStmt
                 in Finally { beforeLoopExit = do
                                 tellFinally
                                 beforeLoopExit finally
                            , beforeReturn = do
                                 tellFinally
                                 beforeReturn finally
                            }
            , maybeCatchLabel
            }
          where
            maybeCatchLabel =
              Just catchLabel

tellExpr :: ( MonadError ContFlowException m
            , MonadReader (R a) m
            , MonadWriter (W a) m
            , Monoid a
            , UniqueMonad m
            ) => Glyph.Expr a -> m ExprIdent
tellExpr = tellExpr'
  where
    tellExpr' (Glyph.Expr a x) =
      localA a $ go x
    go (Glyph.LitE lit) =
      tellE $ LitE lit
    go (Glyph.NotE expr) =
      tellExpr expr >>=
      tellE . NotE
    go (Glyph.VarE name) =
      tellE $ VarE name
    go (Glyph.FunE x params stmts) =
      tellE =<< FunE x params <$> fromFun (map ident params) stmts
    go (Glyph.ApplyE expr exprs) =
      tellE =<< ApplyE <$> tellExpr expr <*> mapM tellExpr exprs
    go (Glyph.AssignE name expr) =
      tellExpr expr >>=
      tellE . AssignE name

class TellS a f | f -> a where
  tellS :: (MonadReader (R a) m, MonadWriter (W a) m, UniqueMonad m) => f -> m ()

instance TellS a (Stmt a O) where
  tellS x =
    tell =<< W . mkMiddle <$> (Stmt <$> askA <*> pure x)

instance TellS a (Successor -> Stmt a C) where
  tellS f = do
    last <- liftM mkLast $ Stmt <$> askA <*> (f <$> asks maybeCatchLabel)
    firstLabel <- freshLabel
    let first = mkFirst $ Label firstLabel
    tell $ W $ last |*><*| first

tellS' :: ( MonadReader (R a) m
          , MonadWriter (W a) m
          , UniqueMonad m
          ) => (forall x . MaybeC x (Label, Label) -> Stmt a x) -> m ()
tellS' = tellS . F

newtype F a = F { unF :: forall x . MaybeC x (Label, Label) -> Stmt a x }

instance TellS a (F a) where
  tellS f = do
    R { annotation = a, maybeCatchLabel } <- ask
    case maybeCatchLabel of
      Nothing ->
        tell $ W $ mkMiddle $ Stmt a $ unF f NothingC
      Just catchLabel -> do
        nextLabel <- freshLabel
        let last = mkLast $ Stmt a $ unF f (JustC (nextLabel, catchLabel))
            first = mkFirst $ Label nextLabel
        tell $ W $ last |*><*| first

tellE :: ( MonadReader (R a) m
         , MonadWriter (W a) m
         , UniqueMonad m
         ) => Expr a -> m ExprIdent
tellE e = do
  x <- freshIdent
  R { annotation = a, maybeCatchLabel } <- ask
  case maybeCatchLabel of
    Nothing ->
      tell $ W $ mkMiddle $ Expr a x e NothingC
    Just catchLabel -> do
      nextLabel <- freshLabel
      let last = mkLast $ Expr a x e $ JustC (nextLabel, catchLabel)
          first = mkFirst $ Label nextLabel
      tell $ W $ last |*><*| first
  return x    

tellLabel :: (MonadReader (R a) m, MonadWriter (W a) m) => Label -> m ()
tellLabel label = do
  last <- getLastS $ GotoS label
  let first = mkFirst $ Label label
  tell $ W $ last |*><*| first

tellGoto :: ( MonadReader (R a) m
            , MonadWriter (W a) m
            , UniqueMonad m
            ) => Label -> m ()
tellGoto label = do
  last <- getLastS $ GotoS label
  firstLabel <- freshLabel
  let first = mkFirst $ Label firstLabel
  tell $ W $ last |*><*| first

getLastS :: MonadReader (R a) m => (Successor -> Stmt a C) -> m (Graph (Insn a) O C)
getLastS f = do
  R { annotation = a, maybeCatchLabel } <- ask
  return $ mkLast $ Stmt a $ f maybeCatchLabel

localLoop :: MonadReader (R a) m => Label -> Label -> m b -> m b
localLoop breakLabel continueLabel =
  local (\ r -> r { finally = (finally r) { beforeLoopExit = return () }
                  , maybeLoopLabels
                  })
  where
    maybeLoopLabels = Just LoopLabels { breakLabel, continueLabel }

askBreakLabel :: (MonadError ContFlowException m, MonadReader (R a) m) => m Label
askBreakLabel = do
  R { maybeLoopLabels } <- ask
  case maybeLoopLabels of
    Nothing ->
      throwError IllegalBreak
    Just (LoopLabels { breakLabel }) ->
      return breakLabel

askContinueLabel :: ( MonadError ContFlowException m
                    , MonadReader (R a) m
                    ) => m Label
askContinueLabel = do
  R { maybeLoopLabels } <- ask
  case maybeLoopLabels of
    Nothing ->
      throwError IllegalContinue
    Just (LoopLabels { continueLabel }) ->
      return continueLabel

localA :: MonadReader (R a) m => a -> m b -> m b
localA a = local (\ r -> r { annotation = a })

askA :: MonadReader (R a) m => m a
askA = asks annotation

data R a
  = R { annotation :: a
      , finally :: forall m .
        ( MonadError ContFlowException m
        , MonadReader (R a) m
        , MonadWriter (W a) m
        , UniqueMonad m
        ) => Finally m
      , maybeCatchLabel :: Maybe Label
      , maybeLoopLabels :: Maybe LoopLabels
      }

data Finally m
  = Finally { beforeLoopExit :: m ()
            , beforeReturn :: m ()
            }

data LoopLabels
  = LoopLabels { breakLabel :: Label
               , continueLabel :: Label
               }

newtype W a = W { unW :: Graph (Insn a) O O }

instance Monoid (W a) where
  mempty = W emptyGraph
  W a `mappend` W b = W $ a |<*>| b
  mconcat = W . catGraphs . map unW

class FreshLabels a where
  freshLabels :: UniqueMonad m => m a

instance FreshLabels Label where
  freshLabels = freshLabel

instance FreshLabels (Label, Label) where
  freshLabels = (,) <$> freshLabel <*> freshLabel

instance FreshLabels (Label, Label, Label) where
  freshLabels = (,,) <$> freshLabel <*> freshLabel <*> freshLabel

(|<*>|) :: NonLocal n => Graph n e O -> Graph n O x -> Graph n e x
(|<*>|) = (Hoopl.<*>)
infixl 3 |<*>|

pure :: Monad m => a -> m a
pure = return

(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap

infixl 4 <$>, <*>
