{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GADTs
  , GeneralizedNewtypeDeriving
  , NamedFieldPuns
  , RankNTypes
  , TypeFamilies #-}
module Language.Glyph.Hoopl
       ( module X
       , HooplException (..)
       , toGraph
       , showGraph'
       ) where

import Compiler.Hoopl hiding ((<*>))
import qualified Compiler.Hoopl as Hoopl
import Control.Comonad
import Control.Exception hiding (block)
import Control.Monad.Error
import qualified Control.Monad.Identity as Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Maybe
import Data.Function
import Data.Typeable

import Language.Glyph.Hoopl.ConstProp
import Language.Glyph.Hoopl.RemoveUnreachable
import Language.Glyph.Hoopl.Simplify
import Language.Glyph.Hoopl.Syntax as X
import Language.Glyph.Ident
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
      , beforeLoopBranch :: forall m .
                            ( MonadError HooplException m
                            , MonadReader (R a) m
                            , UniqueMonad m
                            ) => m (Graph (Stmt a) O O)
      , beforeReturn :: forall m .
                        ( MonadError HooplException m
                        , MonadReader (R a) m
                        , UniqueMonad m
                        ) => m (Graph (Stmt a) O O)
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

localLoop :: MonadReader (R a) m => Label -> Label -> m a' -> m a'
localLoop breakLabel continueLabel =
  local (\ r -> r { beforeLoopBranch = return emptyGraph, maybeLoopLabels })
  where
    maybeLoopLabels = mkLoopLabels breakLabel continueLabel

mkLoopLabels :: Label -> Label -> Maybe (Label, Label)
mkLoopLabels breakLabel continueLabel = Just (breakLabel, continueLabel)

newtype Identity a
  = Identity { unIdentity :: Monad.Identity a
             } deriving Monad

runIdentity :: Identity a -> a
runIdentity = Monad.runIdentity . unIdentity

instance CheckpointMonad Identity where
  type Checkpoint Identity = ()
  checkpoint = return ()
  restart = return

toGraph :: ( Monoid a
           , Monad m
           , MonadError HooplException m
           , UniqueMonad m
           ) => [Glyph.Stmt a] -> m (Graph (Stmt a) O C)
toGraph stmts = do
  graph <- catGraphs <$> runReaderT (mapM fromStmt stmts) r
  let graph' = graph |<*>| mkLast ReturnVoid
  return $ removeUnreachable . runWithFuel' $
    fst3 <$> analyzeAndRewriteFwdOx fwd graph' mempty
  where
    r = R { annotation = mconcat $ map extract stmts
          , beforeLoopBranch = return emptyGraph
          , beforeReturn = return emptyGraph
          , maybeCatchLabel = Nothing
          , maybeLoopLabels = Nothing
          }
    
    runWithFuel' :: InfiniteFuelMonad Identity a -> a
    runWithFuel' = runIdentity . runWithFuel infiniteFuel
    
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x
    
    fwd = FwdPass { fp_lattice = constLattice
                  , fp_transfer = varIsLit
                  , fp_rewrite = simplify
                  }

fromStmt :: ( Monoid a
            , MonadError HooplException m
            , MonadReader (R a) m
            , UniqueMonad m
            ) => Glyph.Stmt a -> m (Graph (Stmt a) O O)
fromStmt (Glyph.Stmt a x) =
  localA a $ case x of
    Glyph.ExprS expr -> do
      (expr', W graph) <- runWriterT $ fromExpr expr
      graph' <- mkMiddle <$> fromStmtView (ExprS expr')
      return $ graph |<*>| graph'
    Glyph.VarDeclS name Nothing ->
      liftM mkMiddle $ fromStmtView $ VarDeclS name Nothing
    Glyph.VarDeclS name (Just expr) -> do
      (expr', W graph) <- runWriterT $ fromExpr expr
      graph' <- mkMiddle <$> fromStmtView (VarDeclS name $ Just expr')
      return $ graph |<*>| graph'
    Glyph.FunDeclS name params stmts ->
      mkMiddle <$> (fromStmtView =<< FunDeclS name params <$> toGraph stmts)
    Glyph.ReturnS Nothing ->
      fromReturnStmt $ fromExprView $ LitE VoidL
    Glyph.ReturnS (Just expr) ->
      fromReturnStmt $ fromExpr expr
    Glyph.IfThenElseS expr stmt Nothing -> do
      (expr', W graph) <- runWriterT $ fromExpr expr
      thenLabel <- freshLabel
      nextLabel <- freshLabel
      if' <- mkLast <$> fromStmtView (IfS expr' thenLabel nextLabel)
      stmt' <- fromStmt stmt
      let then' = mkLabel thenLabel |<*>| stmt' |<*>| mkBranch nextLabel
          next = mkLabel nextLabel
      return $ graph |<*>| if' |*><*| then' |*><*| next
    Glyph.IfThenElseS expr stmt1 (Just stmt2) -> do
      (expr', W graph) <- runWriterT $ fromExpr expr
      thenLabel <- freshLabel
      elseLabel <- freshLabel
      if' <- mkLast <$> fromStmtView (IfS expr' thenLabel elseLabel)
      nextLabel <- freshLabel
      stmt1' <- fromStmt stmt1
      let then' = mkLabel thenLabel |<*>| stmt1' |<*>| mkBranch nextLabel
      stmt2' <- fromStmt stmt2
      let else' = mkLabel elseLabel |<*>| stmt2' |<*>| mkBranch nextLabel
          next = mkLabel nextLabel
      return $ graph |<*>| if' |*><*| then' |*><*| else' |*><*| next
    Glyph.WhileS expr stmt -> do
      testLabel <- freshLabel
      (expr', W graph) <- runWriterT $ fromExpr expr
      bodyLabel <- freshLabel
      nextLabel <- freshLabel
      if' <- mkLast <$> fromStmtView (IfS expr' bodyLabel nextLabel)
      let test = mkLabel testLabel |<*>| graph |<*>| if'
      stmt' <- localLoop nextLabel testLabel $ fromStmt stmt
      let body = mkLabel bodyLabel |<*>| stmt' |<*>| mkBranch testLabel
          next = mkLabel nextLabel
      return $ mkBranch testLabel |*><*| test |*><*| body |*><*| next
    Glyph.BreakS -> do
      graph <- join $ asks beforeLoopBranch
      graph' <- mkBranch <$> askBreakLabel
      graph'' <- mkLabel <$> freshLabel
      return $ graph |<*>| graph' |*><*| graph''
    Glyph.ContinueS -> do
      graph <- join $ asks beforeLoopBranch
      graph' <- mkBranch <$> askContinueLabel
      graph'' <- mkLabel <$> freshLabel
      return $ graph |<*>| graph' |*><*| graph''
    Glyph.ThrowS expr -> do
      (expr', W graph) <- runWriterT $ fromExpr expr
      graph' <- mkLast <$> (fromStmtView =<< ThrowS expr' <$> asks maybeCatchLabel)
      graph'' <- mkLabel <$> freshLabel
      return $ graph |<*>| graph' |*><*| graph''
    Glyph.TryFinallyS stmt Nothing ->
      fromStmt stmt
    Glyph.TryFinallyS  stmt1 (Just stmt2) -> do
      r <- ask
      catchLabel <- freshLabel
      graph <- local (\ r' -> r' { beforeLoopBranch = do
                                      graph <- local (const r) $ fromStmt stmt2
                                      graph' <- beforeLoopBranch r'
                                      return $ graph |<*>| graph'
                                 , beforeReturn = do
                                      graph <- local (const r) $ fromStmt stmt2
                                      graph' <- beforeReturn r'
                                      return $ graph |<*>| graph'
                                 , maybeCatchLabel =
                                      Just catchLabel
                                 }) $ fromStmt stmt1
      nextLabel <- freshLabel
      let graph' = mkBranch nextLabel
          graph'' = mkLabel catchLabel
      graph''' <- fromStmt stmt2
      return $ graph |<*>| graph' |*><*| graph'' |<*>| graph'''
    Glyph.BlockS stmts' ->
      catGraphs <$> mapM fromStmt stmts'
  where
    fromReturnStmt exprM = do
      (expr', W graph) <- runWriterT exprM
      graph' <- join $ asks beforeReturn
      graph'' <- mkLast <$> fromStmtView (ReturnS expr')
      graph''' <- mkLabel <$> freshLabel
      return $ graph |<*>| graph' |<*>| graph'' |*><*| graph'''
      


fromStmtView :: MonadReader (R a) m => StmtView a x -> m (Stmt a O x)
fromStmtView v = do
  a <- askA
  return $ Stmt a v

fromExpr :: ( Monoid a
            , MonadError HooplException m
            , MonadReader (R a) m
            , MonadWriter (W a) m
            , UniqueMonad m
            ) => Glyph.Expr a -> m ExprIdent
fromExpr (Glyph.Expr a v) =
  localA a $ case v of
    Glyph.LitE lit ->
      fromExprView $ LitE lit
    Glyph.NotE expr ->
      fromExprView . NotE =<< fromExpr expr
    Glyph.VarE name ->
      fromExprView $ VarE name
    Glyph.FunE x params stmts ->
      fromExprView . FunE x params =<< toGraph stmts
    Glyph.ApplyE expr exprs -> do
      x <- fromExpr expr
      xs <- mapM fromExpr exprs
      fromExprView $ ApplyE x xs
    Glyph.AssignE name expr ->
      fromExprView . AssignE name =<< fromExpr expr

fromExprView :: ( MonadReader (R a) m
                , MonadWriter (W a) m
                , UniqueMonad m
                ) => ExprView a -> m ExprIdent
fromExprView v = do
  x <- freshIdent
  R { maybeCatchLabel } <- ask
  case maybeCatchLabel of
    Nothing -> do
      a <- askA
      tell $ W $ mkMiddle $ Expr a x v NothingC
    Just catchLabel -> do
      a <- askA
      nextLabel <- freshLabel
      let last = mkLast $ Expr a x v $ JustC (nextLabel, catchLabel)
          first = mkLabel nextLabel
      tell $ W $ last |*><*| first
  return x

localA :: MonadReader (R a) m => a -> m b -> m b
localA a = local (\ r -> r { annotation = a })

askA :: MonadReader (R a) m => m a
askA = asks annotation

showGraph' :: Graph (Stmt a) e x -> String
showGraph' = show . prettyGraph

newtype W a = W (Graph (Stmt a) O O)

instance Monoid (W a) where
  mempty = W emptyGraph
  W a `mappend` W b = W $ a |<*>| b

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