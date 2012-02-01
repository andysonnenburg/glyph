{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GADTs
  , GeneralizedNewtypeDeriving
  , NamedFieldPuns
  , ScopedTypeVariables
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
import Data.Typeable

import Language.Glyph.Hoopl.ConstProp
import Language.Glyph.Hoopl.RemoveUnreachable
import Language.Glyph.Hoopl.Simplify
import Language.Glyph.Hoopl.Syntax as X
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

fromStmt :: forall a m .
            ( Monoid a
            , MonadError HooplException m
            , MonadReader (R a) m
            , UniqueMonad m
            ) => Glyph.Stmt a -> m (Graph (Stmt a) O O)
fromStmt (Glyph.Stmt a x) =
  case x of
    Glyph.ExprS expr -> do
      R { maybeCatchLabel } <- ask
      case maybeCatchLabel of
        Nothing ->
          liftM mkMiddle $ stmtM $
            ExprS <$> fromExpr expr <*> pure NothingC
        Just catchLabel -> do
          nextLabel <- freshLabel
          last <- liftM mkLast $ stmtM $
            ExprS <$> fromExpr expr <*> pure (JustC (nextLabel, catchLabel))
          let first = mkLabel nextLabel
          return $ last |*><*| first
    Glyph.VarDeclS name Nothing ->
      liftM mkMiddle $ stmtM' $
        VarDeclS name UndefinedO
    Glyph.VarDeclS name (Just expr) -> do
      R { maybeCatchLabel } <- ask
      case maybeCatchLabel of
        Nothing ->
          liftM mkMiddle $ stmtM $
            VarDeclS name . ExprO <$> fromExpr expr
        Just catchLabel -> do
          nextLabel <- freshLabel
          stmt <- stmtM $ do
            expr' <- fromExpr expr
            return $ VarDeclS name $ ExprC expr' nextLabel catchLabel
          let last = mkLast stmt
              first = mkLabel nextLabel
          return $ last |*><*| first
    Glyph.FunDeclS name params stmts ->
      liftM mkMiddle $ stmtM $
        FunDeclS name params <$> toGraph stmts
    Glyph.ReturnS expr -> do
      last <- liftM mkLast $ stmtM $ do
        expr' <- maybe (litE VoidL) fromExpr expr
        asks $ ReturnS expr' . maybeCatchLabel
      first <- freshLabel'
      return $ last |*><*| first
    Glyph.IfThenElseS expr stmt Nothing -> do
      thenLabel <- freshLabel
      nextLabel <- freshLabel
      if' <- liftM mkLast $ stmtM $ do
        expr' <- fromExpr expr
        asks $ IfS expr' thenLabel nextLabel . maybeCatchLabel
      stmtGraph <- local' $ fromStmt stmt
      let then' = mkLabel thenLabel |<*>| stmtGraph |<*>| mkBranch nextLabel
          first = mkLabel nextLabel
      return $ if' |*><*| then' |*><*| first
    Glyph.IfThenElseS expr stmt1 (Just stmt2) -> do
      thenLabel <- freshLabel
      elseLabel <- freshLabel
      if' <- liftM mkLast $ stmtM $ do
        expr' <- fromExpr expr
        asks $ IfS expr' thenLabel elseLabel . maybeCatchLabel
      nextLabel <- freshLabel
      stmt1' <- local' $ fromStmt stmt1
      let then' = mkLabel thenLabel |<*>| stmt1' |<*>| mkBranch nextLabel
      stmt2' <- local' $ fromStmt stmt2
      let else' = mkLabel elseLabel |<*>| stmt2' |<*>| mkBranch nextLabel
          first = mkLabel nextLabel
      return $ if' |*><*| then' |*><*| else' |*><*| first
    Glyph.WhileS expr stmt -> do
      testLabel <- freshLabel
      bodyLabel <- freshLabel
      nextLabel <- freshLabel
      if' <- liftM mkLast $ stmtM $ do
        expr' <- fromExpr expr
        asks $ IfS expr' bodyLabel nextLabel . maybeCatchLabel
      let test = mkLabel testLabel |<*>| if'
      stmtGraph <- local' $ localLoopLabels nextLabel testLabel $ fromStmt stmt
      let body = mkLabel bodyLabel |<*>| stmtGraph |<*>| mkBranch testLabel
          next = mkLabel nextLabel
      return $ mkBranch testLabel |*><*| test |*><*| body |*><*| next
    Glyph.BreakS -> do
      last <- mkBranch <$> askBreakLabel
      first <- freshLabel'
      return $ last |*><*| first
    Glyph.ContinueS -> do
      last <- mkBranch <$> askContinueLabel
      first <- freshLabel'
      return $ last |*><*| first
    Glyph.ThrowS expr -> do
      last <- liftM mkLast $ stmtM $ ThrowS <$> fromExpr expr <*> asks maybeCatchLabel
      first <- freshLabel'
      return $ last |*><*| first
    Glyph.BlockS stmts' ->
      local' $ catGraphs <$> mapM fromStmt stmts'
  where
    stmtM :: m (StmtView a x) -> m (Stmt a O x)
    stmtM =
      local' >=> stmtM'
    
    stmtM' :: StmtView a x -> m (Stmt a O x)
    stmtM' =
      return . Stmt a

    freshLabel' =
      mkLabel <$> freshLabel

    local' :: forall a' . m a' -> m a'
    local' =
      local (\ r' -> r' { annotation = a })
    
litE :: MonadReader (R a) m => Lit -> m (Expr a)
litE lit = Expr <$> asks annotation <*> pure (LitE lit)

fromExpr :: ( Monoid a
          , MonadError HooplException m
          , MonadReader (R a) m
          , UniqueMonad m
          ) => Glyph.Expr a -> m (Expr a)
fromExpr (Glyph.Expr a v) =
  exprM $ case v of
    Glyph.LitE lit ->
      pure $ LitE lit
    Glyph.NotE expr ->
      NotE <$> fromExpr expr
    Glyph.VarE name ->
      pure $ VarE name
    Glyph.FunE x params stmts ->
      FunE x params <$> toGraph stmts
    Glyph.ApplyE expr exprs ->
      ApplyE <$> fromExpr expr <*> mapM fromExpr exprs
    Glyph.AssignE name expr ->
      AssignE name <$> fromExpr expr
  where
    exprM =
      liftM (Expr a) . local'
    
    local' =
      local (\ r -> r { annotation = a })

showGraph' :: Graph (Stmt a) e x -> String
showGraph' = show . prettyGraph

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