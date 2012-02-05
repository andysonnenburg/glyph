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
import Control.Exception (Exception)
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

import Prelude hiding (break, catch, last)

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
  local updateR
  where
    updateR r = r { finallyM = (emptyGraphM, snd (finallyM r)), maybeLoopLabels }
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
           , MonadError HooplException m
           , UniqueMonad m
           ) => [Glyph.Stmt a] -> m (Graph (Stmt a) O C)
toGraph = fromFun []

fromFun :: ( Monoid a
           , MonadError HooplException m
           , UniqueMonad m
           ) => [Ident] -> [Glyph.Stmt a] -> m (Graph (Stmt a) O C)
fromFun params stmts = do
  graph <- catGraphs <$> runReaderT (mapM fromStmt stmts) r
  let graph' = graph |<*>| mkLast ReturnVoid
  return $ removeUnreachable . runWithFuel' $
    fst3 <$> analyzeAndRewriteFwdOx fwd graph' (initFact params)
  where
    r = R { annotation = mconcat $ map extract stmts
          , finallyM = (emptyGraphM, emptyGraphM)
          , maybeCatchLabel = Nothing
          , maybeLoopLabels = Nothing
          }

    runWithFuel' :: InfiniteFuelMonad Identity a -> a
    runWithFuel' = runIdentity . runWithFuel infiniteFuel

    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

    fwd = FwdPass { fp_lattice = constLattice
                  , fp_transfer = identIsLit
                  , fp_rewrite = simplify
                  }

fromStmt :: ( Monoid a
            , MonadError HooplException m
            , MonadReader (R a) m
            , UniqueMonad m
            ) => Glyph.Stmt a -> m (Graph (Stmt a) O O)
fromStmt = fromStmt'
  where
    fromStmt' (Glyph.Stmt a x) = localA a $ go x

    go (Glyph.ExprS expr) = do
      (x, W expr') <- runWriterT $ fromExpr expr
      stmt <- mkMiddle <$> fromStmtView (ExprS x)
      return $ expr' |<*>| stmt
    go (Glyph.VarDeclS name Nothing) =
      liftM mkMiddle $ fromStmtView $ VarDeclS name Nothing
    go (Glyph.VarDeclS name (Just expr)) = do
      (x, W expr') <- runWriterT $ fromExpr expr
      varDecl <- mkMiddle <$> fromStmtView (VarDeclS name $ Just x)
      return $ expr' |<*>| varDecl
    go (Glyph.FunDeclS name params stmts) = do
      graph <- fromFun (map ident params) stmts
      mkMiddle <$> fromStmtView (FunDeclS name params graph)
    go (Glyph.ReturnS Nothing) =
      fromReturnStmt $ fromExprView $ LitE VoidL
    go (Glyph.ReturnS (Just expr)) =
      fromReturnStmt $ fromExpr expr
    go (Glyph.IfThenElseS expr stmt Nothing) = do
      (x, W expr') <- runWriterT $ fromExpr expr
      thenLabel <- freshLabel
      nextLabel <- freshLabel
      if' <- mkLast <$> fromStmtView (IfS x thenLabel nextLabel)
      stmt' <- fromStmt stmt
      let then' = mkLabel thenLabel |<*>| stmt' |<*>| mkBranch nextLabel
          next = mkLabel nextLabel
      return $ expr' |<*>| if' |*><*| then' |*><*| next
    go (Glyph.IfThenElseS expr stmt1 (Just stmt2)) = do
      (x, W expr') <- runWriterT $ fromExpr expr
      thenLabel <- freshLabel
      elseLabel <- freshLabel
      if' <- mkLast <$> fromStmtView (IfS x thenLabel elseLabel)
      nextLabel <- freshLabel
      stmt1' <- fromStmt stmt1
      let then' = mkLabel thenLabel |<*>| stmt1' |<*>| mkBranch nextLabel
      stmt2' <- fromStmt stmt2
      let else' = mkLabel elseLabel |<*>| stmt2' |<*>| mkBranch nextLabel
          next = mkLabel nextLabel
      return $ expr' |<*>| if' |*><*| then' |*><*| else' |*><*| next
    go (Glyph.WhileS expr stmt) = do
      testLabel <- freshLabel
      (expr', W graph) <- runWriterT $ fromExpr expr
      bodyLabel <- freshLabel
      nextLabel <- freshLabel
      if' <- mkLast <$> fromStmtView (IfS expr' bodyLabel nextLabel)
      let gotoTest = mkBranch testLabel
          test = mkLabel testLabel |<*>| graph |<*>| if'
      stmt' <- localLoop nextLabel testLabel $ fromStmt stmt
      let body = mkLabel bodyLabel |<*>| stmt' |<*>| gotoTest
          next = mkLabel nextLabel
      return $ gotoTest |*><*| test |*><*| body |*><*| next
    go Glyph.BreakS = do
      finally <- join $ asks $ fst . finallyM
      break <- mkBranch <$> askBreakLabel
      next <- mkLabel <$> freshLabel
      return $ finally |<*>| break |*><*| next
    go Glyph.ContinueS = do
      finally <- join $ asks $ fst . finallyM
      continue <- mkBranch <$> askContinueLabel
      next <- mkLabel <$> freshLabel
      return $ finally |<*>| continue |*><*| next
    go (Glyph.ThrowS expr) = do
      (x, W expr') <- runWriterT $ fromExpr expr
      throw <- mkLast <$> (fromStmtView =<< ThrowS x <$> asks maybeCatchLabel)
      next <- mkLabel <$> freshLabel
      return $ expr' |<*>| throw |*><*| next
    go (Glyph.TryFinallyS stmt Nothing) =
      fromStmt stmt
    go (Glyph.TryFinallyS stmt1 (Just stmt2)) = do
      r <- ask
      let updateR = const r
      catchLabel <- freshLabel
      try <- local (\ r' -> r' { finallyM =
                                      let m = local updateR $ fromStmt stmt2
                                      in (do graph <- m
                                             graph' <- fst $ finallyM r'
                                             return $ graph |<*>| graph',
                                          do graph <- m
                                             graph' <- snd $ finallyM r'
                                             return $ graph |<*>| graph')
                                 , maybeCatchLabel =
                                      Just catchLabel
                                 }) $ fromStmt stmt1
      nextLabel <- freshLabel
      let gotoNext = mkBranch nextLabel
      catch <- fromCatchStmt catchLabel stmt2
      let next = mkLabel nextLabel
      return $ try |<*>| gotoNext |*><*| catch |*><*| next
    go (Glyph.BlockS stmts') =
      catGraphs <$> mapM fromStmt stmts'

    fromReturnStmt exprM = do
      (x, W expr) <- runWriterT exprM
      finally <- join $ asks $ snd . finallyM
      return' <- mkLast <$> fromStmtView (ReturnS x)
      next <- mkLabel <$> freshLabel
      return $ expr |<*>| finally |<*>| return' |*><*| next

    fromCatchStmt catchLabel catchStmt = do
      x <- freshIdent
      let catch = mkFirst $ Catch x catchLabel
      finally <- fromStmt catchStmt
      throw <- mkLast <$> (fromStmtView =<< ThrowS x <$> asks maybeCatchLabel)
      return $ catch |<*>| finally |<*>| throw

fromStmtView :: MonadReader (R a) m => StmtView a x -> m (Stmt a O x)
fromStmtView v = Stmt <$> askA <*> pure v

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
      fromExprView . FunE x params =<< fromFun (map ident params) stmts
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

data R a
  = R { annotation :: a
      , finallyM :: forall m .
                    ( MonadError HooplException m
                    , MonadReader (R a) m
                    , UniqueMonad m
                    ) => (m (Graph (Stmt a) O O), m (Graph (Stmt a) O O))
      , maybeCatchLabel :: Maybe Label
      , maybeLoopLabels :: Maybe (Label, Label)
      }

newtype W a = W (Graph (Stmt a) O O)

instance Monoid (W a) where
  mempty = W emptyGraph
  W a `mappend` W b = W $ a |<*>| b

emptyGraphM :: Monad m => m (Graph (Stmt a) O O)
emptyGraphM = return emptyGraph

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
