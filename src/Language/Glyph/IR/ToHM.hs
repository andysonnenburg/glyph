{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , GADTs
  , RankNTypes
  , ViewPatterns #-}
module Language.Glyph.IR.ToHM
       ( toHM
       ) where

import Compiler.Hoopl
import Control.Monad.Reader

import Data.Monoid

import Language.Glyph.Hoopl
import Language.Glyph.HM.Syntax (Exp, Pat, varP)
import qualified Language.Glyph.HM.Syntax as HM
                                 
import Language.Glyph.Ident
import Language.Glyph.IR.Syntax

toHM :: (Monoid a, UniqueMonad m) => Graph (Insn a) O C -> m (Exp a)
toHM = toExp . toList

toList :: Graph n e x -> [SomeNode n]
toList = foldGraphNodesR f []
  where
    f x xs = SomeNode x : xs

data SomeNode n = forall e x . SomeNode (n e x)

toExp :: (Monoid a, UniqueMonad m) => [SomeNode (Insn a)] -> m (Exp a)
toExp insns = do
  cc <- freshIdent
  let r = initR (foldr f mempty insns) cc
  runReaderT' r $ runCont $ callCC $
    absE (varP cc) (insnsToExp insns)
    where
      f (SomeNode (Stmt a _)) = mappend a
      f (SomeNode (Expr a _ _ _)) = mappend a
      f _ = id

insnsToExp :: Monad m => [SomeNode (Insn a)] -> T a m (Exp a)
insnsToExp insns = vars insns (mapM_' go insns)
  where    
    go (Stmt a stmt) =
      localA a (stmtToExp stmt)
    go (Expr a x expr _) =
      return' (localA a $ varE x `asTypeOf'` exprToExp expr)
    go (Label _) =
      undefined'
    go (Catch x _) =
      return' (varE x)
    go ReturnVoid = do
      cc <- askCC
      appE (varE cc) (litE VoidL)
    
    vars = concatMap' var
      where
        concatMap' f =
          appEndo . mconcat . map (Endo . f)
        var (SomeNode i) =
          case i of
            Stmt a (VarDeclS (ident -> x)) -> localA a . f x
            Expr a x _ _ -> localA a . f x
            Catch x _ -> f x
            _ -> id
          where            
            f x e = do
              a <- askA
              appE (absE (varP x) (localA a e)) undefined'

stmtToExp :: Monad m => Stmt a x -> T a m (Exp a)
stmtToExp = go
  where
    go (ExprS x _) =
      return' $ varE x
    go (VarDeclS (ident -> x)) =
      return' $ varE x
    go (FunDeclS _ _ _) =
      undefined'
    go (ReturnS x _) = do
      cc <- askCC
      appE (varE cc) (varE x)
    go (GotoS _ _) =
      undefined'
    go (IfS x _ _ _) =
      return' $ varE x `asTypeOf'` litE (BoolL True)
    go (ThrowS _ _) =
      undefined'

exprToExp :: Monad m => Expr a -> T a m (Exp a)
exprToExp = go
  where
    go (LitE lit) =
      litE lit
    go (NotE x) =
      varE x `asTypeOf'` litE (BoolL True)
    go (VarE (ident -> x)) =
      varE x
    go (FunE x _ _) =
      varE x
    go (ApplyE x xs) =
      appE (varE x) (tupleE (map varE xs))
    go (AssignE (ident -> x) y) =
      varE x `asTypeOf'` varE y

mapM_' :: Monad m =>
          (forall e x . Insn a e x -> T a m (Exp a)) ->
          [SomeNode (Insn a)] ->
          T a m (Exp a)
mapM_' f as = sequence_' (map f' as)
  where
    f' (SomeNode i) = f i

sequence_' :: Monad m => [T a m (Exp a)] -> T a m (Exp a)
sequence_' ms = foldr then' (return' undefined') ms

varE :: Monad m => Ident -> T a m (Exp a)
varE = liftR0 . HM.varE

appE :: Monad m => T a m (Exp a) -> T a m (Exp a) -> T a m (Exp a)
appE = liftR2 HM.appE

absE :: Monad m => Pat -> T a m (Exp a) -> T a m (Exp a)
absE pat = liftR1 $ HM.absE pat

litE :: Monad m => Lit -> T a m (Exp a)
litE = liftR0 . HM.litE

tupleE :: Monad m => [T a m (Exp a)] -> T a m (Exp a)
tupleE xs = do
  r <- ask
  liftR0 . HM.tupleE . map (lowerR r) $ xs

undefined' :: Monad m => T a m (Exp a)
undefined' = liftR0 HM.undefined'

asTypeOf' :: Monad m => T a m (Exp a) -> T a m (Exp a) -> T a m (Exp a)
asTypeOf' = liftR2 HM.asTypeOf'

return' :: Monad m => T a m (Exp a) -> T a m (Exp a)
return' = liftR1 HM.return'

then' :: Monad m => T a m (Exp a) -> T a m (Exp a) -> T a m (Exp a)
then' = liftR2 HM.then'

runCont :: Monad m => T a m (Exp a) ->  T a m (Exp a)
runCont = liftR1 HM.runCont

callCC :: Monad m => T a m (Exp a) -> T a m (Exp a)
callCC = liftR1 HM.callCC

liftR0 :: Monad m => ReaderT r m a -> T r m a
liftR0 = withReaderT annotation

liftR1 :: Monad m => (ReaderT r m a -> ReaderT r m a) -> T r m a -> T r m a
liftR1 f m = do
  r <- ask
  liftR0 $ f (lowerR r m)

liftR2 :: Monad m =>
          (ReaderT r m a -> ReaderT r m a -> ReaderT r m a) ->
          T r m a -> T r m a -> T r m a
liftR2 f m n = do
  r <- ask
  liftR0 $ f (lowerR r m) (lowerR r n)

lowerR :: R r -> T r m a -> ReaderT r m a
lowerR r = withReaderT (\ a -> r { annotation = a })
      
type T a = ReaderT (R a)

initR :: a -> Ident -> R a
initR = R

data R a
  = R { annotation :: a
      , ccIdent :: Ident
      }

askA :: MonadReader (R a) m => m a
askA = asks annotation

localA :: MonadReader (R r) m => r -> m a -> m a
localA a = local (\ r -> r { annotation = a})

askCC :: MonadReader (R a) m => m Ident
askCC = asks ccIdent

runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT