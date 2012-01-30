{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GADTs
  , NamedFieldPuns
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Hoopl
       ( module X
       , Stmt (..)
       , StmtView (..)
       , Expr (..)
       , ExprView (..)
       , HooplException (..)
       , toGraph
       , prettyGraph
       , showGraph'
       ) where

import Compiler.Hoopl
import Control.Comonad
import Control.Exception hiding (block)
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Maybe
import Data.Typeable

import Language.Glyph.Hoopl.Live
import Language.Glyph.Hoopl.RemoveUnreachable
import Language.Glyph.Hoopl.Syntax
import Language.Glyph.Syntax as X hiding (Stmt,
                                          StmtView (..),
                                          Expr,
                                          ExprView (..))
import qualified Language.Glyph.Syntax.Internal as Glyph

import Prelude hiding (last)

data HooplException
  = IllegalBreak
  | IllegalContinue
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show HooplException where
  show x =
    case x of
      IllegalBreak -> "illegal break statement"
      IllegalContinue -> "illegal continue statement"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception HooplException

instance Error HooplException where
  strMsg = StrMsgError
  noMsg = NoMsgError

data R a
  = R { annotation :: a
      , maybeCatchLabel :: Maybe Label
      , maybeLoopLabels :: Maybe (Label, Label)
      }

askBreakLabel :: (MonadError HooplException m, MonadReader (R a) m) => m Label
askBreakLabel = do
  R { maybeLoopLabels } <- ask
  maybe (throwError IllegalBreak) (return . fst) maybeLoopLabels

askContinueLabel :: (MonadError HooplException m, MonadReader (R a) m) => m Label
askContinueLabel = do
  R { maybeLoopLabels } <- ask
  maybe (throwError IllegalContinue) (return . snd) maybeLoopLabels

localLoopLabels :: MonadReader (R a) m => Label -> Label -> m a' -> m a'
localLoopLabels breakLabel continueLabel = local (\ r -> r { maybeLoopLabels })
  where
    maybeLoopLabels = mkLoopLabels breakLabel continueLabel

mkLoopLabels :: Label -> Label -> Maybe (Label, Label)
mkLoopLabels breakLabel continueLabel = Just (breakLabel, continueLabel)

instance UniqueMonad m => UniqueMonad (ReaderT r m) where
  freshUnique = lift freshUnique

instance (Monoid w, UniqueMonad m) => UniqueMonad (WriterT w m) where
  freshUnique = lift freshUnique

instance CheckpointMonad Identity where
  type Checkpoint Identity = ()
  checkpoint = return ()
  restart = return

toGraph :: forall a m .
           ( Monoid a
           , Monad m
           , MonadError HooplException m
           , UniqueMonad m
           ) => [Glyph.Stmt a] -> m (Graph (Stmt a) O C)
toGraph stmts = do
  graph <- liftM catGraphs $ runReaderT (mapM go stmts) r
  let graph' = graph <*> mkLast ReturnVoid
  return $
    removeUnreachable . fst3 . runWithFuel' $
    analyzeAndRewriteBwdOx bwd graph' mapEmpty
  where
    r = R { annotation = mconcat $ map extract stmts
          , maybeCatchLabel = Nothing
          , maybeLoopLabels = Nothing
          }
    
    runWithFuel' :: forall a . InfiniteFuelMonad Identity a -> a
    runWithFuel' = runIdentity . runWithFuel infiniteFuel
    
    fst3 (x, _, _) = x
    
    bwd = BwdPass { bp_lattice = liveLattice
                  , bp_transfer = liveness
                  , bp_rewrite = deadAssignElim
                  }
    
    go :: forall m' .
          ( MonadError HooplException m'
          , MonadReader (R a) m'
          , UniqueMonad m'
          ) => Glyph.Stmt a -> m' (Graph (Stmt a) O O)
    go (Glyph.Stmt a x) =
      case x of
        Glyph.ExprS expr -> do
          R { maybeCatchLabel } <- ask
          case maybeCatchLabel of
            Nothing -> do
              stmt <- stmtM $ do
                expr' <- toExpr expr
                return $ ExprS expr' NothingC
              let middle = mkMiddle stmt
              return middle
            Just catchLabel -> do
              nextLabel <- freshLabel
              stmt <- stmtM $ do
                expr' <- toExpr expr
                return $ ExprS expr' $ JustC (nextLabel, catchLabel)
              let last = mkLast stmt
                  first = mkLabel nextLabel
              return $ last |*><*| first
        Glyph.VarDeclS name Nothing ->
          liftM mkMiddle $ stmtM' $ VarDeclS name UndefinedO
        Glyph.VarDeclS name (Just expr) -> do
          R { maybeCatchLabel } <- ask
          case maybeCatchLabel of
            Nothing -> do
              stmt <- stmtM $ do
                expr' <- toExpr expr
                return $ VarDeclS name $ ExprO expr'
              let middle = mkMiddle stmt
              return middle
            Just catchLabel -> do
              nextLabel <- freshLabel
              stmt <- stmtM $ do
                expr' <- toExpr expr
                return $ VarDeclS name $ ExprC expr' nextLabel catchLabel
              let last = mkLast stmt
                  first = mkLabel nextLabel
              return $ last |*><*| first
        Glyph.FunDeclS name params stmts' -> do
          liftM mkMiddle $ stmtM $ do
            graph <- toGraph stmts'
            return $ FunDeclS name params graph
        Glyph.ReturnS expr -> do
          last <- liftM mkLast $ stmtM $ do
            expr' <- maybe (litE VoidL) toExpr expr
            R { maybeCatchLabel } <- ask
            return $ ReturnS expr' maybeCatchLabel
          nextLabel <- freshLabel
          let first = mkLabel nextLabel
          return $ last |*><*| first
        Glyph.IfThenElseS expr stmt Nothing -> do
          thenLabel <- freshLabel
          nextLabel <- freshLabel
          if' <- liftM mkLast $ stmtM $ do
            expr' <- toExpr expr
            asks $ IfS expr' thenLabel nextLabel . maybeCatchLabel
          stmtGraph <- localA $ go stmt
          let then' = mkLabel thenLabel <*> stmtGraph <*> mkBranch nextLabel
              first = mkLabel nextLabel
          return $ if' |*><*| then' |*><*| first
        Glyph.IfThenElseS expr stmt1 (Just stmt2) -> do
          thenLabel <- freshLabel
          elseLabel <- freshLabel
          if' <- liftM mkLast $ stmtM $ do
            expr' <- toExpr expr
            asks $ IfS expr' thenLabel elseLabel . maybeCatchLabel
          nextLabel <- freshLabel
          stmt1Graph <- localA $ go stmt1
          let then' = mkLabel thenLabel <*> stmt1Graph <*> mkBranch nextLabel
          stmt2Graph <- localA $ go stmt2
          let else' = mkLabel elseLabel <*> stmt2Graph <*> mkBranch nextLabel
              first = mkLabel nextLabel
          return $ if' |*><*| then' |*><*| else' |*><*| first
        Glyph.WhileS expr stmt -> do
          testLabel <- freshLabel
          bodyLabel <- freshLabel
          nextLabel <- freshLabel
          if' <- liftM mkLast $ stmtM $ do
            expr' <- toExpr expr
            asks $ IfS expr' bodyLabel nextLabel . maybeCatchLabel
          let test = mkLabel testLabel <*> if'
          stmtGraph <- localA $ localLoopLabels nextLabel testLabel $ go stmt
          let body = mkLabel bodyLabel <*> stmtGraph <*> mkBranch testLabel
              next = mkLabel nextLabel
          return $ mkBranch testLabel |*><*| test |*><*| body |*><*| next
        Glyph.BreakS -> do
          breakLabel <- askBreakLabel
          let last = mkBranch breakLabel
          label <- freshLabel
          let first = mkLabel label
          return $ last |*><*| first
        Glyph.ContinueS -> do
          continueLabel <- askContinueLabel
          let last = mkBranch continueLabel
          label <- freshLabel
          let first = mkLabel label
          return $ last |*><*| first
        Glyph.ThrowS expr -> do
          stmt <- stmtM $ do
            expr' <- toExpr expr
            R { maybeCatchLabel } <- ask
            return $ ThrowS expr' maybeCatchLabel
          let last = mkLast stmt
          nextLabel <- freshLabel
          let first = mkLabel nextLabel
          return $ last |*><*| first
        Glyph.BlockS stmts' ->
          localA $ liftM catGraphs $ mapM go stmts'
      where
        stmtM :: forall x . m' (StmtView a x) -> m' (Stmt a O x)
        stmtM m = do
          x' <- localA m
          return $ Stmt a x'
        
        stmtM' =
          stmtM . return
        
        localA :: forall a' . m' a' -> m' a'
        localA m = do
          local (\ r' -> r' { annotation = a }) m
    
litE :: MonadReader (R a) m => Lit -> m (Expr a)
litE lit = do
  R { annotation = a } <- ask
  return $ Expr a (LitE lit)

toExpr :: ( Monoid a
          , MonadError HooplException m
          , MonadReader (R a) m
          , UniqueMonad m
          ) => Glyph.Expr a -> m (Expr a)
toExpr (Glyph.Expr a v) =
  exprM $ case v of
    Glyph.LitE lit ->
      return $ LitE lit
    Glyph.NotE expr -> do
      expr' <- toExpr expr
      return $ NotE expr'
    Glyph.VarE name ->
      return $ VarE name
    Glyph.FunE x params stmts -> do
     graph <- toGraph stmts
     return $ FunE x params graph
    Glyph.ApplyE expr exprs -> do
      expr' <- toExpr expr
      exprs' <- mapM toExpr exprs
      return $ ApplyE expr' exprs'
    Glyph.AssignE name expr -> do
      expr' <- toExpr expr
      return $ AssignE name expr'
  where
    exprM m = do
      x' <- local (\ r -> r { annotation = a }) m
      return $ Expr a x'

showGraph' :: Graph (Stmt a) e x -> String
showGraph' = show . prettyGraph