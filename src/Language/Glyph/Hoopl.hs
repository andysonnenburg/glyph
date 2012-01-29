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
       , prettyGraph
       , showGraph'
       ) where

import Compiler.Hoopl
import Control.Comonad
import Control.Exception hiding (block)
import Control.Monad.Error
import Control.Monad.Reader
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
  ExprS :: Expr a -> MaybeC x (Label, Label) -> StmtView a x
  -- TODO XXX
  VarDeclS :: Name -> Maybe (Expr a, MaybeC x (Label, Label)) -> StmtView a x
  FunDeclS :: Name -> [Name] -> Graph (Stmt a) O C -> StmtView a O
  ReturnS :: Expr a -> Maybe Label -> StmtView a C
  IfS :: Expr a -> Label -> Label -> Maybe Label -> StmtView a C
  ThrowS :: Expr a -> Maybe Label -> StmtView a C

instance Show (StmtView a x) where
  show = show . pretty

instance Pretty (Stmt a e x) where
  pretty = go
    where
      go :: Stmt a e x -> Doc e'
      go (Stmt _ x) =
        pretty x
      go (Label label) =
        prettyLabel label <> colon
      go (Goto label) =
        text "goto" <+> prettyLabel label <> semi
      go ReturnVoid =
        text "return" <+> pretty VoidL <> semi

instance Pretty (StmtView a x) where
  pretty = go
    where
      go :: StmtView a x -> Doc e
      go (VarDeclS name Nothing) =
        varDecl name
      go (VarDeclS name (Just (expr, NothingC))) =
        varDef name expr
      go (VarDeclS name (Just (expr, (JustC (nextLabel, catchLabel))))) =
        varDef name expr
        `above`
        text "goto" <+> prettyLabel nextLabel <> semi
      go (FunDeclS name params graph) =
        text "fn" <+> pretty name <> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (IfS expr then' else' catch') =
        text "if" <+> parens (pretty expr) <+>
        prettyLabel then' <+>
        prettyLabel else' <>
        semi
      
      varDef :: Name -> Expr a -> Doc e
      varDef name expr =
        var name <+> char '=' <+> pretty expr <> semi
      
      varDecl :: Name -> Doc e
      varDecl name =
        var name <> semi
      
      var :: Name -> Doc e
      var name =
        text "var" <+> pretty name

instance Pretty (Expr a) where
  pretty (Expr _ x) = pretty x

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

data Expr a = Expr a (ExprView a) deriving Show

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
          , maybeCatchLabel = Nothing
          , maybeLoopLabels = Nothing
          }
    
    go :: forall m .
          ( MonadError HooplException m
          , MonadReader (R a) m
          , UniqueMonad m
          ) => Glyph.Stmt a -> m (Graph (Stmt a) O O)
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
        Glyph.VarDeclS name (Just expr) -> do
          R { maybeCatchLabel } <- ask
          case maybeCatchLabel of
            Nothing -> do
              stmt <- stmtM $ do
                expr' <- toExpr expr
                return $ VarDeclS name (Just (expr', NothingC))
              let middle = mkMiddle stmt
              return middle
        Glyph.VarDeclS name Nothing ->
          liftM mkMiddle $ stmtM' $ VarDeclS name Nothing
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
          ifGraph <- liftM mkLast $ stmtM $ do
            expr' <- toExpr expr
            R { maybeCatchLabel } <- ask
            return $ IfS expr' thenLabel nextLabel maybeCatchLabel
          stmtGraph <- go stmt
          let thenGraph = mkLabel thenLabel <*> stmtGraph <*> mkBranch nextLabel
          return $ ifGraph |*><*| thenGraph |*><*| mkLabel nextLabel
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

prettyGraph :: Graph (Stmt a) e x -> Doc e'
prettyGraph = go
  where
    go :: Graph (Stmt a) e x -> Doc e'
    go GNil =
      empty
    go (GUnit unit) =
      vcat $ block unit []
    go (GMany entry blocks exit) =
      vcat $
      open (flip block []) entry ++
      body blocks ++
      open (flip block []) exit
    
    open :: (a -> [Doc e]) -> MaybeO z a -> [Doc e]
    open _ NothingO = []
    open p (JustO x) = p x
    
    body :: LabelMap (Block (Stmt a) C C) -> [Doc e]
    body blocks =
      concatMap (flip block []) . mapElems $ blocks
    
    block :: forall a e x e' .
             Block (Stmt a) e x ->
             IndexedCO x [Doc e'] [Doc e'] ->
             IndexedCO e [Doc e'] [Doc e']
    block = foldBlockNodesB f
      where
        f :: forall e x . Stmt a e x -> [Doc e'] -> [Doc e']
        f stmt docs = pretty stmt : docs

showGraph' :: Graph (Stmt a) e x -> String
showGraph' = show . prettyGraph