{-# LANGUAGE
    ConstraintKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , RankNTypes
  , TypeFamilies #-}
module Language.Glyph.IR
       ( module X
       , toGraph
       , showGraph'
       ) where

import Compiler.Hoopl hiding ((<*>))
import qualified Compiler.Hoopl as Hoopl
import Control.Comonad
import Control.Exception
import Control.Monad.Error
import qualified Control.Monad.Identity as Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Monoid
import Data.Typeable

import Language.Glyph.Ident
import qualified Language.Glyph.IR.Operand
import Language.Glyph.IR.Syntax as X
import Language.Glyph.Syntax.Internal
import Language.Glyph.Syntax as X hiding (Stmt,
                                          StmtView (..),
                                          Expr,
                                          ExprView (..))

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

toGraph :: ( Monoid a
           , MonadError ContFlowException m
           , UniqueMonad m
           ) => [Stmt a] -> m (Graph (Insn a) O C)
toGraph = fromFun []

showGraph' :: Graph (Insn a) e x -> String
showGraph' = show . prettyGraph

fromFun :: ( Monoid a
           , MonadError ContFlowException m
           , UniqueMonad m
           ) => [Ident] -> [Stmt a] -> m (Graph (Insn a) O C)
fromFun params stmts = do
  graph <- mconcat <$> runReaderT (mapM fromStmt stmts) r
  let graph' = graph |<*>| mkLast ReturnVoid
  return graph'
  where
    r = R { annotation = mconcat $ map extract stmts
          , finallyM = (emptyGraphM, emptyGraphM)
          , maybeCatchLabel = Nothing
          , maybeLoopLabels = Nothing
          }

fromStmt :: Stmt a -> m (Graph (Insn a) O O)
fromStmt = liftM unW . execWriterT . tellStmt

tellStmt :: ( Monoid a
            , MonadError ContFlowException m
            , MonadReader (R a) m
            , MonadWriter (W a) m
            , UniqueMonad m
            ) => Stmt a -> m ()
tellStmt = tellStmt'
  where
    tellStmt' (Stmt a x) = localA a $ go x
    
    go (ExprS expr) = do
      tellExpr expr
      tellInsnView Pop
    go (VarDeclS name Nothing) =
      tellInsnView $ VarDecl name
    go (ReturnS Nothing) = do
      tellInsnView $ Lit VoidL
      tellInsnView Return
    go (IfThenElseS expr stmt Nothing) = do
      tellExpr expr
      thenLabel <- freshLabel
      nextLabel <- freshLabel
      if' <- fromInsnView $ If thenLabel nextLabel
      let then' = mkFirst $ Label thenLabel
      stmt' <- fromStmt stmt
      gotoNext <- fromInsnView $ Goto nextLabel
      let next = mkFirst $ Label nextLabel
      tellGraph $ if' |*><*| then' |<*>| stmt' |<*>| gotoNext |*><*| next
    go (BlockS stmts) =
      mapM_ tellStmt stmts

fromExpr :: Expr a -> m (Graph (Insn a) O O)
fromExpr = liftM unW . execWriterT . tellExpr

tellExpr :: MonadWriter (W a) m => Expr a -> m ()
tellExpr = undefined

fromInsnView :: TellInsnView a f => f -> m (Graph (Insn a) O O)
fromInsnView = liftM unW . execWriterT . tellInsnView

class ToGraph a f | f -> a where
  type MonadConstraint m :: Constraint
  toGraph :: MonadConstraint m => f -> m (Graph (Insn a) O O)
  tellGraph :: ( MonadReader (R a) m
               , MonadWriter (W a) m
               ) => f -> m ()

instance TellInsnView a (Successors x -> InsnView a x) where
  tellInsnView = undefined

instance TellInsnView a (InsnView a O) where
  tellInsnView x = askA >>= tellGraph . mkMiddle . flip Insn x

instance TellInsnView a (Successor -> InsnView a C) where
  tellInsnView = undefined

tellGraph :: MonadWriter (W a) m => Graph (Insn a) O O -> m ()
tellGraph = tell . W

emptyGraphM :: Monad m => m (Graph (Insn a) O O)
emptyGraphM = return emptyGraph

localA :: MonadReader (R a) m => a -> m b -> m b
localA a = local (\ r -> r { annotation = a })

askA :: MonadReader (R a) m => m a
askA = asks annotation

newtype Identity a
  = Identity { unIdentity :: Monad.Identity a
             } deriving Monad

runIdentity :: Identity a -> a
runIdentity = Monad.runIdentity . unIdentity

instance CheckpointMonad Identity where
  type Checkpoint Identity = ()
  checkpoint = return ()
  restart = return

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