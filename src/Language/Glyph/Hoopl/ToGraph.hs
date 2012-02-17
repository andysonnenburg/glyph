{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RankNTypes
  , TypeSynonymInstances #-}
module Language.Glyph.Hoopl.ToGraph
       ( ContFlowException
       , toGraph
       ) where

import Control.Comonad
import Control.Exception
import Compiler.Hoopl hiding ((<*>), mkFirst, mkMiddle, mkLast)
import qualified Compiler.Hoopl as Hoopl
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Typeable

import Language.Glyph.Hoopl.RemoveUnreachable
import Language.Glyph.Hoopl.Syntax
import Language.Glyph.Ident
import qualified Language.Glyph.Syntax.Internal as Glyph

import Prelude hiding (last)

toGraph :: ( Monoid a
           , MonadError ContFlowException m
           , UniqueMonad m
           ) => [Glyph.Stmt a] -> m (Graph (Insn a) O C)
toGraph = fromFun []

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
          , finallyM = (emptyGraphM, emptyGraphM)
          , maybeCatchLabel = Nothing
          , maybeLoopLabels = Nothing
          }

class TellGraph a b f | f -> a b where
  tellGraph :: ( Monoid a
               , MonadError ContFlowException m
               , MonadReader (R a) m
               , MonadWriter (W a) m
               , UniqueMonad m
               ) => f -> m b

toGraph' :: ( Monoid a
            , MonadError ContFlowException m
            , MonadReader (R a) m
            , TellGraph a () f
            , UniqueMonad m
            ) => f -> m (Graph (Insn a) O O)
toGraph' =
  liftM unW . execWriterT . tellGraph

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

instance Error ContFlowException where
  strMsg = StrMsgError
  noMsg = NoMsgError

instance TellGraph a () (Graph (Insn a) O O) where
  tellGraph =
    tell . W

instance TellGraph a () (Insn a O O) where
  tellGraph =
    tell . W . mkMiddle

instance TellGraph a ExprIdent (Expr a) where
  tellGraph e = do
    x <- freshIdent
    R { annotation = a, maybeCatchLabel } <- ask
    case maybeCatchLabel of
      Nothing ->
        tellGraph $ Expr a x e NothingC
      Just catchLabel -> do
        nextLabel <- freshLabel
        let last = mkLast $ Expr a x e $ JustC (nextLabel, catchLabel)
            first = mkFirst $ Label nextLabel
        tellGraph $ last |*><*| first
    return x

instance TellGraph a () [Glyph.Stmt a] where
  tellGraph = mapM_ tellGraph

instance TellGraph a () (Glyph.Stmt a) where
  tellGraph = fromStmt'
    where
      fromStmt' (Glyph.Stmt a x) = localA a $ go x
      
      go (Glyph.ExprS expr) = do
        x <- tellExpr expr
        tellStmt $ ExprS x
      go (Glyph.VarDeclS name Nothing) = do
        tellStmt $ F $ VarDeclS name Nothing
      go (Glyph.VarDeclS name (Just expr)) = do
        x <- tellExpr expr
        tellStmt $ F $ VarDeclS name $ Just x
      go (Glyph.FunDeclS name params stmts) =
        fromFun (map ident params) stmts >>=
        tellStmt . FunDeclS name params
      go (Glyph.ReturnS maybeExpr) = do
        x <- tellExpr $ fromMaybe (toExpr LitE VoidL) maybeExpr
        tellStmt $ ReturnS x
      go (Glyph.IfThenElseS expr stmt Nothing) = do
        x <- tellExpr expr
        thenLabel <- freshLabel
        nextLabel <- freshLabel
        if' <- fromStmt $ IfS x thenLabel nextLabel
        let then' = mkFirst $ Label thenLabel
        stmt' <- fromStmt stmt
        gotoNext <- fromStmt $ GotoS nextLabel
        let next = mkFirst $ Label nextLabel
        tellGraph $ if' |*><*| then' |<*>| stmt' |<*>| gotoNext |*><*| next
      go (Glyph.BlockS stmts) =
        mapM_ tellStmt stmts

instance TellGraph a ExprIdent (Glyph.Expr a) where
  tellGraph (Glyph.Expr a v) =
    localA a $ case v of
      Glyph.LitE lit ->
        tellGraph $ LitE lit
      Glyph.NotE expr ->
        tellGraph . NotE =<< tellGraph expr
      Glyph.VarE name ->
        tellGraph $ VarE name
      Glyph.FunE x params stmts ->
        tellGraph . FunE x params =<< fromFun (map ident params) stmts
      Glyph.ApplyE expr exprs -> do
        x <- tellGraph expr
        xs <- mapM tellGraph exprs
        tellGraph $ ApplyE x xs
      Glyph.AssignE name expr ->
        tellGraph . AssignE name =<< tellGraph expr

instance TellGraph a () (Stmt' a) where
  tellGraph (Stmt' f) = do
    R { annotation = a, maybeCatchLabel } <- ask
    case maybeCatchLabel of
      Nothing ->
        tellGraph $ Stmt a $ f NothingC
      Just catchLabel -> do
        nextLabel <- freshLabel
        let stmt = mkLast $ Stmt a $ f $ JustC (nextLabel, catchLabel)
            next = mkFirst $ Label nextLabel
        tellGraph $ stmt |*><*| next

instance TellGraph a () (Stmt a O) where
  tellGraph x = do
    a <- askA
    tellGraph $ Stmt a x

instance TellGraph a () (Successor -> Stmt a C) where
  tellGraph f = do
    R { annotation = a, maybeCatchLabel } <- ask
    let stmt = mkLast $ Stmt a $ f maybeCatchLabel
    next <- mkFirst . Label <$> freshLabel
    tellGraph $ stmt |*><*| next

tellStmt :: MonadWriter (W a) m => Glyph.Stmt a -> m ()

class FromStmt a x f | f -> a x where
  fromStmt :: f -> Graph (Insn a) O x

newtype F a b = F { unF :: forall x . a x -> b x }

instance FromStmt a O (F Successors (Stmt a)) where
  fromStmt (F f) = do
    R { annotation = a, maybeCatchLabel } <- askA
    case maybeCatchLabel of
      Nothing ->
        return $ mkMiddle $ Stmt a $ f NothingC
      Just catchLabel -> do
        nextLabel <- freshLabel
        let stmt = mkLast $ Stmt a $ f $ JustC (nextLabel, catchLabel)
            next = mkFirst $ Label nextLabel
        return $ stmt |*><*| next

instance FromStmt a C (Successor -> Stmt a C)

instance FromStmt a O (Stmt a O)

class TellExpr a f | f -> a where
  tellExpr :: MonadWriter (W a) m => f -> m ExprIdent

instance TellExpr a (Glyph.Expr a) where
  tellExpr = undefined

instance TellExpr a (Expr a) where
  tellExpr = undefined

tellExpr :: Expr a -> m ExprIdent

toExpr :: Glyph.Expr a -> m (Expr a)

localA :: MonadReader (R a) m => a -> m b -> m b
localA a = local (\ r -> r { annotation = a })

askA :: MonadReader (R a) m => m a
askA = asks annotation

data R a
  = R { annotation :: a
      , finallyM :: forall m .
                    ( MonadError ContFlowException m
                    , MonadReader (R a) m
                    , UniqueMonad m
                    ) => (m (Graph (Insn a) O O), m (Graph (Insn a) O O))
      , maybeCatchLabel :: Maybe Label
      , maybeLoopLabels :: Maybe (Label, Label)
      }

newtype W a = W { unW :: Graph (Insn a) O O }

instance Monoid (W a) where
  mempty = W emptyGraph
  W a `mappend` W b = W $ a |<*>| b
  mconcat = W . catGraphs . map unW

emptyGraphM :: Monad m => m (Graph (Insn a) O O)
emptyGraphM = return emptyGraph

mkFirst :: Insn a C O -> Graph (Insn a) C O
mkFirst = Hoopl.mkFirst

mkMiddle :: Insn a O O -> Graph (Insn a) O O
mkMiddle = Hoopl.mkMiddle

mkLast :: Insn a O C -> Graph (Insn a) O C
mkLast = Hoopl.mkLast

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