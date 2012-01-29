{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GADTs
  , NamedFieldPuns
  , ScopedTypeVariables
  , StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Hoopl
       ( module X
       , Stmt (..)
       , StmtView (..)
       , Expr (..)
       , ExprView (..)
       , HooplException (..)
       , toGraph
       , showGraph'
       ) where

import Control.Comonad
import Control.Exception hiding (block)
import Compiler.Hoopl
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Typeable

import Language.Glyph.Ident
import Language.Glyph.Syntax as X hiding (Stmt,
                                          StmtView (..),
                                          Expr,
                                          ExprView (..))
import qualified Language.Glyph.Syntax.Internal as Glyph

import Text.PrettyPrint.Free

import Prelude hiding (last)

data Stmt a e x where
  Stmt :: a -> StmtView a x -> Stmt a O x
  Label :: Label -> Stmt a C O
  Goto :: Label -> Stmt a O C
  Try :: Stmt a O O
  EndTry :: MaybeC x Label -> Stmt a O x
  Catch :: Maybe Name -> Label -> Stmt a C O
  ReturnVoid :: Stmt a O C

instance Show (Stmt a e x) where
  show = show . pretty

data StmtView a x where
  ExprS :: Expr a -> Label -> StmtView a C
  VarDeclS :: Name -> MaybeC x (Expr a, Label) -> StmtView a x
  FunDeclS :: Name -> [Name] -> Graph (Stmt a) O C -> StmtView a O
  ReturnS :: Expr a -> Maybe Label -> StmtView a C
  IfS :: Expr a -> Label -> Label -> StmtView a C
  ThrowS :: Expr a -> Maybe Label -> StmtView a C

instance Show (StmtView a x) where
  show = show . pretty

instance Pretty (Stmt a e x) where
  pretty x = go x
    where
      go :: Stmt a e x -> Doc e'
      go (Stmt _ view) =
        pretty view
      go (Label label) =
        prettyLabel label <> char ':'
      go (Goto label) =
        text "goto" <+> prettyLabel label <> char ';'
      go ReturnVoid =
        text "return" <+> pretty VoidL <> char ';'

instance Pretty (StmtView a x) where
  pretty = go
    where
      go :: StmtView a x -> Doc e
      go (VarDeclS name NothingC) =
        text "var" <+> pretty name <> char ';'
      go (VarDeclS name (JustC (expr, label))) =
        text "var" <+> pretty name <+> char '=' <+> pretty expr <> char ';'
        `above`
        text "goto" <+> prettyLabel label <> char ';'
      go (IfS expr then' else') =
        text "if" <+> parens (pretty expr) <+>
        prettyLabel then' <+>
        prettyLabel else'

instance Pretty (Expr a) where
  pretty (Expr _ view Nothing) = pretty view

instance Pretty (ExprView a) where
  pretty = go
    where
      go (LitE lit) =
        pretty lit

prettyLabel :: Label -> Doc e'
prettyLabel = text . show

instance NonLocal (Stmt a) where
  entryLabel (Label label) = label
  
  successors (Goto label) = [label]
  successors ReturnVoid = []

instance HooplNode (Stmt a) where
  mkBranchNode = Goto
  mkLabelNode = Label

data Expr a = Expr a (ExprView a) (Maybe Label) deriving Show

data ExprView a where
  LitE :: Lit -> ExprView a
  NotE :: Expr a -> ExprView a
  VarE :: Name -> ExprView a
  FunE :: Ident -> [Name] -> Graph (Stmt a) O C -> ExprView a
  ApplyE :: Expr a -> [Expr a] -> ExprView a
  AssignE :: Name -> Expr a -> ExprView a

instance Show a => Show (ExprView a) where

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
      , catchLabel :: Maybe Label
      , loopLabels :: Maybe (Label, Label)
      }

askBreakLabel :: (MonadError HooplException m, MonadReader (R a) m) => m Label
askBreakLabel = do
  R { loopLabels } <- ask
  maybe (throwError IllegalBreak) (return . fst) loopLabels

askContinueLabel :: (MonadError HooplException m, MonadReader (R a) m) => m Label
askContinueLabel = do
  R { loopLabels } <- ask
  maybe (throwError IllegalContinue) (return . snd) loopLabels

mkLoopLabels :: Label -> Label -> Maybe (Label, Label)
mkLoopLabels breakLabel continueLabel = Just (breakLabel, continueLabel)

instance UniqueMonad m => UniqueMonad (ReaderT r m) where
  freshUnique = lift freshUnique

instance (Monoid w, UniqueMonad m) => UniqueMonad (WriterT w m) where
  freshUnique = lift freshUnique

toGraph :: forall a m .
           ( Monoid a
           , Monad m
           , MonadError HooplException m
           , UniqueMonad m
           ) => [Glyph.Stmt a] -> m (Graph (Stmt a) O C)
toGraph stmts = do
  graph <- liftM catGraphs $ runReaderT (mapM go stmts) r
  return $ graph <*> mkLast ReturnVoid
  where
    r = R { annotation = mconcat $ map extract stmts
          , catchLabel = Nothing
          , loopLabels = Nothing
          }
    
    go :: forall m .
          ( MonadError HooplException m
          , MonadReader (R a) m
          , UniqueMonad m
          ) => Glyph.Stmt a -> m (Graph (Stmt a) O O)
    go (Glyph.Stmt a x) =
      case x of
        Glyph.ExprS expr -> do
          nextLabel <- freshLabel
          last <- liftM mkLast $ stmtM $ do
            expr' <- toExpr expr
            return $ ExprS expr' nextLabel
          let first = mkLabel nextLabel
          return $ last |*><*| first
        Glyph.VarDeclS name (Just expr) -> do
          nextLabel <- freshLabel
          last <- liftM mkLast $ stmtM $ do
            expr' <- toExpr expr
            return $ VarDeclS name (JustC (expr', nextLabel))
          let first = mkLabel nextLabel
          return $ last |*><*| first
        Glyph.VarDeclS name Nothing ->
          liftM mkMiddle $ stmtM' $ VarDeclS name NothingC
        Glyph.FunDeclS name params stmts' -> do
          liftM mkMiddle $ stmtM $ do
            graph <- toGraph stmts'
            return $ FunDeclS name params graph
        Glyph.ReturnS expr -> do
          last <- liftM mkLast $ stmtM $ do
            expr' <- maybe (litE VoidL) toExpr expr
            R { catchLabel } <- ask
            return $ ReturnS expr' catchLabel
          nextLabel <- freshLabel
          let first = mkLabel nextLabel
          return $ last |*><*| first
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
        Glyph.BlockS stmts' ->
          liftM catGraphs $ mapM go stmts'
      where
        stmtM :: forall x . m (StmtView a x) -> m (Stmt a O x)
        stmtM m = do
          x' <- local (\ r' -> r' { annotation = a }) m
          return $ Stmt a x'
        
        stmtM' =
          stmtM . return
    
litE :: MonadReader (R a) m => Lit -> m (Expr a)
litE lit = do
  R { annotation = a, catchLabel } <- ask
  return $ Expr a (LitE lit) catchLabel

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
      R { catchLabel } <- ask
      return $ Expr a x' catchLabel

showGraph' :: Graph (Stmt a) e x -> String
showGraph' = showGraph (show . (<> line) . pretty)
