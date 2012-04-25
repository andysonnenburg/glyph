{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , NamedFieldPuns
  , NoMonomorphismRestriction
  , RankNTypes
  , RebindableSyntax
  , ScopedTypeVariables
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
import Control.Monad.Error (Error (..), MonadError)
import Control.Monad.Reader (MonadReader, runReaderT)

import Data.Foldable (toList)
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup
import Data.Typeable

import Language.Glyph.Generics
import Language.Glyph.Hoopl
import Language.Glyph.Hoopl.Monad.Writer
import Language.Glyph.IR.Syntax
import Language.Glyph.Ident
import qualified Language.Glyph.Syntax as Glyph
import Language.Glyph.Unique ()

import Text.PrettyPrint.Free

import Prelude hiding (Monad (..), (=<<), init, last)

fromStmts :: forall a m .
             ( Data a
             , Semigroup a
             , MonadError ContFlowException m
             , UniqueMonad m
             ) => [Glyph.Stmt a] -> m (Module Unlifted a)
fromStmts =
  liftM mkModule .
  maybe fromEmpty fromNonEmpty . nonEmpty
  where
    return = Monad.return
    (>>=) = (Monad.>>=)

    mkModule init = Module (Object NothingL init NothingL)

    runReaderT' = flip runReaderT
    
    fromEmpty =
      return $ Init (mkLast ReturnVoid) mempty (JustU mempty)
    
    fromNonEmpty stmts = do
      runReaderT' r $ mainM (toList stmts)
      where
        insnsM =
          liftM unW' .
          execWriterT .
          mapM_ tellStmt

        mainM :: ( MonadError ContFlowException m'
                 , MonadReader (R a) m'
                 , UniqueMonad m'
                 ) => [Glyph.Stmt a] -> m' (Init Unlifted a)
        mainM stmts' = do
          insns <- insnsM stmts'
          let vars = varsQ stmts'
          funs <- funsM stmts'
          return $ Init insns vars (JustU funs)

        varsQ =
          everythingButFuns (<>) (mempty `mkQ` f)
          where
            f :: Glyph.StmtView a -> [Ident]
            f (Glyph.VarDeclS name _) = [ident name]
            f _ = mempty

        funsM =
          funsQ funM

        funM :: ( MonadError ContFlowException m'
                , MonadReader (R a) m'
                , UniqueMonad m'
                ) => Ident -> [Ident] -> [Glyph.Stmt a] -> m' (Fun Unlifted a)
        funM x params stmts' = do
          insns <- insnsM stmts'
          let vars = varsQ stmts'
          funs <- funsM stmts'
          return $ Fun x params insns vars (JustU funs)

        funsQ :: forall b m' .
                 Monad.Monad m' =>
                 (Ident -> [Ident] -> [Glyph.Stmt a] -> m' b) ->
                 [Glyph.Stmt a] ->
                 m' [b]
        funsQ f =
          sequence . everythingButFuns (<>) (mempty `mkQ` s `extQ` e)
          where
            s :: Glyph.StmtView a -> [m' b]
            s (Glyph.FunDeclS name params stmts') =
              [f (ident name) (map ident params) stmts']
            s _ =
              mempty
            e :: Glyph.ExprView a -> [m' b]
            e (Glyph.FunE x params stmts') =
              [f x (map ident params) stmts']
            e _ =
              mempty

        unW' w =
          unW w |<*>| mkLast ReturnVoid

        r = R { annotation = sconcat . fmap extract $ stmts
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
  show = show . pretty

instance Pretty ContFlowException where
  pretty = go
    where
      go IllegalBreak =
        text "illegal" </>
        text "break" </>
        text "statement"
      go IllegalContinue =
        text "illegal" </>
        text "continue" </>
        text "statement"
      go (StrMsgError s) =
        text s
      go NoMsgError =
        text "internal" </>
        text "error"

instance Exception ContFlowException

instance Error ContFlowException where
  strMsg = StrMsgError
  noMsg = NoMsgError

tellStmt :: forall a m .
            ( MonadError ContFlowException m
            , MonadReader (R a) m
            , Semigroup a
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
    go (Glyph.VarDeclS _name Nothing) =
      return ()
    go (Glyph.VarDeclS name (Just expr)) = do
      x <- tellExprInsn . AssignE (ident name) =<< tellExpr expr
      tellInsn $ F $ ExprS x
    go (Glyph.FunDeclS _name _params _stmts) =
      return ()
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

    localCatchLabel catchLabel =
      local (\ r -> r { maybeCatchLabel = Just catchLabel })

    localFinally finallyStmt m = do
      (nextLabel, catchLabel) <- freshLabels
      local f $ localCatchLabel catchLabel $ do
        _ <- m
        tellInsn $ GotoS nextLabel
      x <- freshIdent
      tellInsn $ Catch x catchLabel
      tellStmt finallyStmt
      tellInsn $ ThrowS x
      tellInsn $ Label nextLabel
      tellStmt finallyStmt
      where
        f r@R { finally } =
          r { finally = finally' }
          where
            finally' :: forall m' .
                        ( MonadError ContFlowException m'
                        , MonadReader (R a) m'
                        , UniqueMonad m'
                        ) => Finally (WriterT (W a) m')
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

tellExpr :: ( MonadError ContFlowException m
            , MonadReader (R a) m
            , Semigroup a
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
      tellExprInsn $ VarE . ident $ name
    go (Glyph.FunE x _params _stmts) =
      tellExprInsn $ VarE x
    go (Glyph.ApplyE expr exprs) =
      tellExprInsn =<<
      ApplyE <$>
      tellExpr expr <*>
      mapM tellExpr exprs
    go (Glyph.ApplyMethodE expr methodName exprs) =
      tellExprInsn =<<
      ApplyMethodE <$>
      tellExpr expr <*>
      pure methodName <*>
      mapM tellExpr exprs
    go (Glyph.AssignE name expr) =
      tellExprInsn . AssignE (ident name) =<< tellExpr expr

class TellInsn a e x f | f -> a e x where
  tellInsn :: (MonadReader (R a) m, UniqueMonad m) => f -> WriterT (W a) m e x ()

instance TellInsn a C O (Insn a C O) where
  tellInsn = tell . mkW . mkFirst

instance TellInsn a O O (Insn a O O) where
  tellInsn = tell . mkW . mkMiddle

instance TellInsn a O C (Insn a O C) where
  tellInsn = tell . mkW . mkLast

instance TellInsn a O O (Stmt O) where
  tellInsn x =
    tellInsn =<< Stmt <$> askA <*> pure x

instance TellInsn a O C (Successor -> Stmt C) where
  tellInsn f = tellInsn =<< Stmt <$> askA <*> (f <$> asks maybeCatchLabel)

newtype F = F { unF :: forall x . MaybeC x (Label, Label) -> Stmt x }

instance TellInsn a O O F where
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
                ) => Expr -> WriterT (W a) m O O ExprIdent
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
