{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RankNTypes
  , RebindableSyntax
  , ScopedTypeVariables
  , TypeSynonymInstances
  , UndecidableInstances #-}
module Language.Glyph.IR.FromStmts
       ( ContFlowException (..)
       , fromStmts
       ) where

import Control.Comonad
import Control.Exception hiding (finally)
import Compiler.Hoopl hiding ((<*>))
import qualified Compiler.Hoopl as Hoopl
import Control.Monad (ap, join, liftM)
import qualified Control.Monad as Monad
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader (MonadReader, runReaderT)

import Data.Monoid (Monoid)
import qualified Data.Monoid as Monoid
import Data.Typeable

import Language.Glyph.Hoopl
import Language.Glyph.Hoopl.Monad.Writer
import Language.Glyph.IR.RemoveUnreachable
import Language.Glyph.IR.Syntax
import Language.Glyph.Ident
import qualified Language.Glyph.Syntax.Internal as Glyph

import Prelude hiding (Monad (..), (=<<), last)

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
  graph <- liftM unW . runReaderT' r . execWriterT . mapM_ tellStmt $ stmts
  let graph' = graph |<*>| mkLast ReturnVoid
  return $ removeUnreachable graph'
  where
    (>>=) = (Monad.>>=)
    return = Monad.return
    
    runReaderT' = flip runReaderT
    
    r = R { annotation = Monoid.mconcat $ map extract stmts
          , finally = initFinally
          , maybeCatchLabel = Nothing
          , maybeLoopLabels = Nothing
          }

initFinally :: Monad m => Finally m
initFinally = Finally { tellBeforeLoopExit
                      , tellBeforeReturn
                      }
  where
    tellBeforeLoopExit =
      return ()
    tellBeforeReturn =
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

tellStmt :: forall a m .
            ( MonadError ContFlowException m
            , MonadReader (R a) m
            , Monoid a
            , UniqueMonad m
            ) => Glyph.Stmt a -> WriterT (W a) m O O ()
tellStmt = tellStmt'
  where
    tellStmt' (Glyph.Stmt a x) =
      localA a $ go x
    go :: Glyph.StmtView a -> WriterT (W a) m O O ()
    go (Glyph.ExprS e) = do
      x <- tellExpr e
      tellInsn $ F $ ExprS x
    go (Glyph.VarDeclS name Nothing) =
      tellInsn $ VarDeclS name
    go (Glyph.VarDeclS name (Just expr)) = do
      tellInsn $ VarDeclS name
      x <- tellExprInsn . AssignE name =<< tellExpr expr
      tellInsn $ F $ ExprS x
    go (Glyph.FunDeclS name params stmts) =
      tellInsn =<< FunDeclS name params <$> fromFun (map ident params) stmts
    go (Glyph.ReturnS Nothing) = do
      x <- tellExprInsn $ LitE VoidL
      join $ asks $ tellBeforeReturn . finally
      tellInsn $ ReturnS x
      nextLabel <- freshLabel
      tellInsn $ Label nextLabel
    go (Glyph.ReturnS (Just expr)) = do
      x <- tellExpr expr
      join $ asks $ tellBeforeReturn . finally
      nextLabel <- freshLabel
      tellInsn $ ReturnS x
      tellInsn $ Label nextLabel
    go (Glyph.IfThenElseS expr stmt Nothing) = do
      x <- tellExpr expr
      (thenLabel, nextLabel) <- freshLabels
      tellInsn $ IfS x thenLabel nextLabel
      tellInsn $ Label thenLabel
      tellStmt stmt
      tellInsn $ GotoS nextLabel
      tellInsn $ Label nextLabel
    go (Glyph.IfThenElseS expr stmt1 (Just stmt2)) = do
      x <- tellExpr expr
      (thenLabel, elseLabel, nextLabel) <- freshLabels
      tellInsn $ IfS x thenLabel elseLabel
      tellInsn $ Label thenLabel
      tellStmt stmt1
      tellInsn $ GotoS nextLabel
      tellInsn $ Label elseLabel
      tellStmt stmt2
      tellInsn $ GotoS nextLabel
      tellInsn $ Label nextLabel
    go (Glyph.WhileS expr stmt) = do
      testLabel <- freshLabel
      tellInsn $ GotoS testLabel
      tellInsn $ Label testLabel
      x <- tellExpr expr
      (thenLabel, nextLabel) <- freshLabels
      tellInsn $ IfS x thenLabel nextLabel
      tellInsn $ Label thenLabel
      localLoop nextLabel testLabel $ tellStmt stmt
      tellInsn $ GotoS testLabel
      tellInsn $ Label nextLabel
    go Glyph.BreakS = do
      join $ asks $ tellBeforeLoopExit . finally
      tellInsn . GotoS =<< askBreakLabel
      nextLabel <- freshLabel
      tellInsn $ Label nextLabel
    go Glyph.ContinueS = do
      join $ asks $ tellBeforeLoopExit . finally
      tellInsn . GotoS =<< askContinueLabel
      nextLabel <- freshLabel
      tellInsn $ Label nextLabel
    go (Glyph.ThrowS expr) = do
      x <- tellExpr expr
      tellInsn $ ThrowS x
      nextLabel <- freshLabel
      tellInsn $ Label nextLabel
    go (Glyph.TryFinallyS stmt Nothing) =
      tellStmt stmt
    go (Glyph.TryFinallyS stmt1 (Just stmt2)) =
      localFinally stmt2 $ tellStmt stmt1
    go (Glyph.BlockS stmts) =
      mapM_ tellStmt stmts
    
    localFinally finallyStmt m = do
      local f $ localCatch finallyStmt m
      tellStmt finallyStmt
      where
        f r@R { finally } =
          r { finally = finally' }
          where
            finally' :: forall m .
                        ( MonadError ContFlowException m
                        , MonadReader (R a) m
                        , UniqueMonad m
                        ) => Finally (WriterT (W a) m) 
            finally' =
              Finally { tellBeforeLoopExit = do
                           tellFinally
                           tellBeforeLoopExit finally
                      , tellBeforeReturn = do
                           tellFinally
                           tellBeforeReturn finally
                      }
              where
                tellFinally =
                  local (const r) $ tellStmt finallyStmt
    
    localCatch catchStmt m = do
      (nextLabel, catchLabel) <- freshLabels
      local (\ r -> r { maybeCatchLabel = Just catchLabel }) $ do
        m
        tellInsn $ GotoS nextLabel
      x <- freshIdent
      tellInsn $ Catch x catchLabel
      tellStmt catchStmt
      tellInsn $ ThrowS x
      tellInsn $ Label nextLabel

tellExpr :: ( MonadError ContFlowException m
            , MonadReader (R a) m
            , Monoid a
            , UniqueMonad m
            ) => Glyph.Expr a -> WriterT (W a) m O O ExprIdent
tellExpr = tellExpr'
  where
    tellExpr' (Glyph.Expr a x) =
      localA a $ go x
    go (Glyph.LitE lit) =
      tellExprInsn $ LitE lit
    go (Glyph.NotE expr) =
      tellExprInsn . NotE =<< tellExpr expr
    go (Glyph.VarE name) =
      tellExprInsn $ VarE name
    go (Glyph.FunE x params stmts) =
      tellExprInsn =<< FunE x params <$> fromFun (map ident params) stmts
    go (Glyph.ApplyE expr exprs) =
      tellExprInsn =<< ApplyE <$> tellExpr expr <*> mapM tellExpr exprs
    go (Glyph.AssignE name expr) =
      tellExprInsn . AssignE name =<< tellExpr expr

class TellInsn a e x f | f -> a e x where
  tellInsn :: (MonadReader (R a) m, UniqueMonad m) => f -> WriterT (W a) m e x ()

instance TellInsn a C O (Insn a C O) where
  tellInsn = tell . mkW . mkFirst

instance TellInsn a O O (Insn a O O) where
  tellInsn = tell . mkW . mkMiddle

instance TellInsn a O C (Insn a O C) where
  tellInsn = tell . mkW . mkLast

instance TellInsn a O O (Stmt a O) where
  tellInsn x =
    tellInsn =<< Stmt <$> askA <*> pure x

instance TellInsn a O C (Successor -> Stmt a C) where
  tellInsn f = tellInsn =<< Stmt <$> askA <*> (f <$> asks maybeCatchLabel)

newtype F a = F { unF :: forall x . MaybeC x (Label, Label) -> Stmt a x }

instance TellInsn a O O (F a) where
  tellInsn f = do
    R { annotation = a, maybeCatchLabel } <- ask
    case maybeCatchLabel of
      Nothing ->
        tellInsn $ Stmt a $ unF f NothingC
      Just catchLabel -> do
        nextLabel <- freshLabel
        tellInsn $ Stmt a $ unF f (JustC (nextLabel, catchLabel))
        tellInsn $ Label nextLabel

tellExprInsn :: ( MonadReader (R a) m
                , UniqueMonad m
                ) => Expr a -> WriterT (W a) m O O ExprIdent
tellExprInsn expr = do
  R { annotation = a, maybeCatchLabel } <- ask
  x <- freshIdent
  case maybeCatchLabel of
    Nothing ->
      tellInsn $ Expr a x expr NothingC
    Just catchLabel -> do
      nextLabel <- freshLabel
      tellInsn $ Expr a x expr $ JustC (nextLabel, catchLabel)
      tellInsn $ Label nextLabel
  return x

localLoop :: MonadReader (R a) m =>
             Label -> Label -> WriterT (W a) m ex ex b -> WriterT (W a) m ex ex b
localLoop breakLabel continueLabel =
  local (\ r@R { finally } ->
          r { finally = finally { tellBeforeLoopExit = return () }
            , maybeLoopLabels
            })
  where
    maybeLoopLabels = Just LoopLabels { breakLabel, continueLabel }

askBreakLabel :: ( MonadError ContFlowException m
                 , MonadReader (R a) m
                 ) => WriterT (W a) m ex ex Label
askBreakLabel = do
  R { maybeLoopLabels } <- ask
  case maybeLoopLabels of
    Nothing ->
      throwError IllegalBreak
    Just (LoopLabels { breakLabel }) ->
      return breakLabel

askContinueLabel :: ( MonadError ContFlowException m
                    , MonadReader (R a) m
                    ) => WriterT (W a) m ex ex Label
askContinueLabel = do
  R { maybeLoopLabels } <- ask
  case maybeLoopLabels of
    Nothing ->
      throwError IllegalContinue
    Just (LoopLabels { continueLabel }) ->
      return continueLabel

localA :: MonadReader (R a) m => a -> WriterT (W a) m ex ex b -> WriterT (W a) m ex ex b
localA a = local (\ r -> r { annotation = a })

askA :: MonadReader (R a) m => WriterT (W a) m ex ex a
askA = asks annotation

data R a
  = R { annotation :: a
      , finally :: forall m .
        ( MonadError ContFlowException m
        , MonadReader (R a) m
        , UniqueMonad m
        ) => Finally (WriterT (W a) m)
      , maybeCatchLabel :: Maybe Label
      , maybeLoopLabels :: Maybe LoopLabels
      }

data Finally m
  = Finally { tellBeforeLoopExit :: m O O ()
            , tellBeforeReturn :: m O O ()
            }

data LoopLabels
  = LoopLabels { breakLabel :: Label
               , continueLabel :: Label
               }

class FreshLabels a where
  freshLabels :: UniqueMonad m => m a

instance FreshLabels Label where
  freshLabels = freshLabel

instance FreshLabels (Label, Label) where
  freshLabels = (,) <$> freshLabel <*> freshLabel

instance FreshLabels (Label, Label, Label) where
  freshLabels = (,,) <$> freshLabel <*> freshLabel <*> freshLabel

type W a = WrappedSemigroupoid (WrappedGraph (Insn a))

mkW :: Graph (Insn a) e x -> W a e x
mkW = Semi . WrapGraph

unW :: W a O O -> Graph (Insn a) O O
unW = unwrapGraph . unwrapSemigroupoid (WrapGraph emptyGraph)

(|<*>|) :: NonLocal n => Graph n e O -> Graph n O x -> Graph n e x
(|<*>|) = (Hoopl.<*>)
infixl 3 |<*>|

pure :: Monad m => a -> m ex ex a
pure = return

infixl 4 <$>, <*>

(<$>) :: Monad.Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

(<*>) :: Monad.Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
