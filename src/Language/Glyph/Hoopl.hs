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

import Data.Maybe
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
  Catch :: Maybe Name -> Label -> Stmt a C O
  ReturnVoid :: Stmt a O C

instance Show (Stmt a e x) where
  show = show . pretty

data StmtView a x where
  ExprS :: Expr a -> MaybeC x (Label, Label) -> StmtView a x
  VarDeclS :: Name -> VarInit a x -> StmtView a x
  FunDeclS :: Name -> [Name] -> Graph (Stmt a) O C -> StmtView a O
  ReturnS :: Expr a -> Maybe Label -> StmtView a C
  IfS :: Expr a -> Label -> Label -> Maybe Label -> StmtView a C
  ThrowS :: Expr a -> Maybe Label -> StmtView a C

data VarInit a x where
  UndefinedO :: VarInit a O
  ExprO :: Expr a -> VarInit a O
  ExprC :: Expr a -> Label -> Label -> VarInit a C

data Expr a = Expr a (ExprView a)

data ExprView a where
  LitE :: Lit -> ExprView a
  NotE :: Expr a -> ExprView a
  VarE :: Name -> ExprView a
  FunE :: Ident -> [Name] -> Graph (Stmt a) O C -> ExprView a
  ApplyE :: Expr a -> [Expr a] -> ExprView a
  AssignE :: Name -> Expr a -> ExprView a


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
      go (Catch Nothing _label) =
        text "catch" <+> parens (text "...") <> colon
      go (Catch (Just name) _label) =
        text "catch" <+> parens (pretty name) <> colon
      go ReturnVoid =
        text "return" <+> pretty VoidL <> semi

instance Pretty (StmtView a x) where
  pretty = go
    where
      go :: StmtView a x -> Doc e
      go (ExprS expr NothingC) =
        pretty expr <> semi
      go (ExprS expr (JustC (nextLabel, _catchLabel))) =
        pretty expr <> semi
        `above`
        goto nextLabel
      go (VarDeclS name UndefinedO) =
        varDecl name
      go (VarDeclS name (ExprO expr)) =
        varDef name expr
      go (VarDeclS name (ExprC expr nextLabel _catchLabel)) =
        varDef name expr
        `above`
        goto nextLabel
      go (FunDeclS name params graph) =
        text "fn" <+> pretty name <> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (ReturnS expr _maybeCatchLabel) =
        text "return" <+> pretty expr <> semi 
      go (IfS expr then' else' _maybeCatchLabel) =
        text "if" <+> parens (pretty expr) <+>
        prettyLabel then' <+>
        prettyLabel else' <>
        semi
      go (ThrowS expr _maybeCatchLabel) =
        text "throw" <+> pretty expr <> semi
      
      varDef :: Name -> Expr a -> Doc e
      varDef name expr =
        var name <+> char '=' <+> pretty expr <> semi
      
      varDecl :: Name -> Doc e
      varDecl name =
        var name <> semi
      
      var :: Name -> Doc e
      var name =
        text "var" <+> pretty name

      goto :: Label -> Doc e
      goto label =
        text "goto" <+> prettyLabel label <> semi

instance Pretty (Expr a) where
  pretty (Expr _ x) = pretty x

instance Pretty (ExprView a) where
  pretty = go
    where
      go (LitE lit) =
        pretty lit
      go (NotE expr) =
        char '!' <> pretty expr
      go (VarE name) =
        pretty name
      go (FunE _ params graph) =
        text "fn" <+> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (ApplyE expr exprs) =
        pretty expr <> tupled (map pretty exprs)
      go (AssignE name expr) =
        pretty name <+> char '=' <+> pretty expr

prettyLabel :: Label -> Doc e'
prettyLabel = text . show

instance NonLocal (Stmt a) where
  entryLabel = go
    where
      go (Label label) = label
      go (Catch _maybeName label) = label
  
  
  successors = stmtSuccessors
    where
      stmtSuccessors = go
        where
          go (Stmt _ x) = stmtViewSuccessors x
          go (Goto label) = [label]
          go ReturnVoid = []
      
      stmtViewSuccessors = go
        where
          go :: StmtView a C -> [Label]
          go (ExprS _ (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (VarDeclS _ (ExprC _ nextLabel catchLabel)) =
            [nextLabel, catchLabel]
          go (ReturnS _ maybeCatchLabel') =
            maybeToList maybeCatchLabel'
          go (IfS _ thenLabel elseLabel maybeCatchLabel') =
            [thenLabel, elseLabel] ++ maybeToList maybeCatchLabel'

instance HooplNode (Stmt a) where
  mkBranchNode = Goto
  mkLabelNode = Label

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
        stmtM :: forall x . m (StmtView a x) -> m (Stmt a O x)
        stmtM m = do
          x' <- localA m
          return $ Stmt a x'
        
        stmtM' =
          stmtM . return
        
        localA :: forall a . m a -> m a
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