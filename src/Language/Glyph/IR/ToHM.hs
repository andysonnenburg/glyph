{-# LANGUAGE
    DataKinds
  , ExistentialQuantification
  , FlexibleContexts
  , GADTs
  , RankNTypes
  , ScopedTypeVariables
  , ViewPatterns #-}
module Language.Glyph.IR.ToHM
       ( toHM
       ) where

import Compiler.Hoopl hiding (Label)
import Control.Monad.Reader

import Data.Graph (SCC (..), stronglyConnCompR)
import Data.Monoid

import Language.Glyph.Generics
import Language.Glyph.Hoopl
import Language.Glyph.HM.Syntax (Exp, Label)
import qualified Language.Glyph.HM.Syntax as HM
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import Language.Glyph.IdentSet (IdentSet)
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.IR.Syntax
import Language.Glyph.Record hiding (Module, Symtab, insns, select)
import qualified Language.Glyph.Record as Record
import Language.Glyph.Unique ()

import Prelude hiding (init)

toHM :: ( Monoid a
        , Typeable a
        , Select CallSet IdentSet sym
        , Select Record.Symtab (Symtab sym) fields
        , Select Record.Module (Module Unlifted a) fields
        , UniqueMonad m
        ) => Record fields -> m (Exp a)
toHM r = toExp (r#.symtab) init
  where
    Module (Object NothingL init NothingL) = r#.module'

toList :: Graph n e x -> [SomeNode n]
toList = foldGraphNodesR f []
  where
    f x xs = SomeNode x : xs

data SomeNode n = forall e x . SomeNode !(n e x)

type SomeInsn a = SomeNode (Insn a)

toExp :: ( Monoid a
         , Typeable a
         , Select CallSet IdentSet sym
         , UniqueMonad m
         ) => Symtab sym -> Init Unlifted a -> m (Exp a)
toExp m (Init (toList -> insns) vars (JustU funs)) = do
  cc <- freshIdent
  let r = initR (mconcatInsns insns) cc m
  runReaderT' r $ do
    x <- freshIdent
    absE x $
      declareList vars $
      funsToExp funs $
      runCont $ callCC $
        absE cc (insnsToExp insns
                 `then'`
                 appE (varE cc) (litE VoidL))

funToExp :: ( Monoid a
            , Typeable a
            , Select CallSet IdentSet sym
            , UniqueMonad m
            ) => Fun Unlifted a -> T a sym m (Exp a)
funToExp (Fun _x params (toList -> insns) vars (JustU funs)) =
  localA (mconcatInsns insns) $ do
    x <- freshIdent
    absE x $
      selectList x params $
      declareList vars $
      funsToExp funs $
      runCont $ callCC $ do
        cc <- freshIdent
        absE cc (localCC cc $ insnsToExp insns)

funsToExp :: ( Monoid a
             , Typeable a
             , Select CallSet IdentSet sym
             , UniqueMonad m
             ) => [Fun Unlifted a] -> T a sym m (Exp a) -> T a sym m (Exp a)
funsToExp funs e' = do
  scc <- liftM stronglyConnCompR . mapM mkNode $ funs
  localA (mconcatFuns funs) $ foldr' letE' scc e'
  where
    mkNode f@(Fun x _params _insns _vars _funs) = do
      m <- askSymtab
      let xs = IdentSet.toList $ (m ! x)#.callSet
      return (f, x, xs)
    foldr' f as b =
      foldr f b as

letE' :: ( Typeable a
         , Monoid a
         , Select CallSet IdentSet sym
         , UniqueMonad m
         ) =>
         SCC (Fun Unlifted a, Ident, [Ident]) ->
         T a sym m (Exp a) ->
         T a sym m (Exp a)
letE' (AcyclicSCC (funToExp -> e, x, _)) e' =
  letE x e e'
letE' (CyclicSCC [(funToExp -> e, x, _)]) e' =
  letE x (fix' (absE x e)) e'
letE' (CyclicSCC vertices) e' = do
  x <- freshIdent
  y <- freshIdent
  letE x (fix' (absE y $ selectList y xs $ tupleE e)) $
    selectList x xs e'
  where
    xs = map snd' vertices
    e = map (funToExp . fst') vertices
    fst' (x, _, _) = x
    snd' (_, x, _) = x

mconcatFuns :: Monoid a => [Fun Unlifted a] -> a
mconcatFuns = mconcat . map f
  where
    f (Fun _x _params (toList -> insns) _vars (JustU funs)) =
      mconcatInsns insns <> mconcatFuns funs

mconcatInsns :: Monoid a => [SomeInsn a] -> a
mconcatInsns = foldr f mempty
  where
    f (SomeNode (Stmt a _)) = mappend a
    f (SomeNode (Expr a _ _ _)) = mappend a
    f _ = id

insnsToExp :: Monad m => [SomeInsn a] -> T a sym m (Exp a)
insnsToExp = go'
  where
    go' (SomeNode x:xs) =
      go x xs
    go' [] =
      return' undefined'
    go :: Monad m => Insn a e x -> [SomeInsn a] -> T a sym m (Exp a)
    go (Stmt a stmt) insns =
      localA a (stmtToExp stmt)
      `then''`
      insnsToExp insns
    go (Expr a x expr _) insns =
      localA a $ letE x (exprToExp expr) (insnsToExp insns)
    go (Label _) insns =
      insnsToExp insns
    go (Catch x _) insns =
      letE x undefined' (insnsToExp insns)
    go ReturnVoid insns = do
      cc <- askCC
      appE (varE cc) (litE VoidL)
      `then'`
      insnsToExp insns
    e1 `then''` e2 =
      maybe e2 ((`then'` e2) . return) =<< e1

stmtToExp :: Monad m => Stmt x -> T a sym m (Maybe (Exp a))
stmtToExp = go
  where
    go (ExprS {}) =
      return Nothing
    go (ReturnS x _) = liftM Just $ do
      cc <- askCC
      appE (varE cc) (varE x)
    go (GotoS {}) =
      return Nothing
    go (IfS x _ _ _) = liftM Just $
      return' $ varE x `asTypeOf'` litE (BoolL True)
    go (ThrowS {}) =
      return Nothing

exprToExp :: Monad m => Expr -> T a sym m (Exp a)
exprToExp = go
  where
    go (LitE lit) =
      litE lit
    go (NotE x) =
      varE x `asTypeOf'` litE (BoolL True)
    go (VarE x) =
      varE x
    go (ApplyE x xs) =
      appE (varE x) (tupleE (map varE xs))
    go (BindE x xs l) =
      appE (bind (length xs) l) (tupleE (map varE (x:xs)))
    go (ApplyMethodE x method xs) =
      appE (accessE method (varE x)) (tupleE (map varE xs))
    go (AssignE x y) =
      varE x `asTypeOf'` varE y

varE :: Monad m => Ident -> T a sym m (Exp a)
varE = liftR0 . HM.varE

appE :: Monad m => T a sym m (Exp a) -> T a sym m (Exp a) -> T a sym m (Exp a)
appE = liftR2 HM.appE

absE :: Monad m => Ident -> T a sym m (Exp a) -> T a sym m (Exp a)
absE x = liftR1 $ HM.absE x

letE :: Monad m => Ident -> T a sym m (Exp a) -> T a sym m (Exp a) -> T a sym m (Exp a)
letE x = liftR2 $ HM.letE x

litE :: Monad m => Lit -> T a sym m (Exp a)
litE = liftR0 . HM.litE

tupleE :: Monad m => [T a sym m (Exp a)] -> T a sym m (Exp a)
tupleE xs = do
  r <- ask
  liftR0 . HM.tupleE . map (lowerR r) $ xs

selectList :: UniqueMonad m => Ident -> [Ident] -> T a sym m (Exp a) -> T a sym m (Exp a)
selectList x = go
  where
    go [] e = do
      y <- freshIdent
      letE y (varE x `asTypeOf'` tupleE []) e
    go ys e = foldr f id (zip [0 ..] ys) e
      where
        f (i, y) f' = letE y (appE (select i l) (varE x)) . f'
        l = length ys

declareList :: Monad m => [Ident] -> T a sym m (Exp a) -> T a sym m (Exp a)
declareList = go
  where
    go = foldr f id
      where
        f x f' e = appE (absE x (f' e)) undefined'

select :: Monad m => Int -> Int -> T a sym m (Exp a)
select i l = liftR0 $ HM.select i l

accessE :: Monad m => Label -> T a sym m (Exp a) -> T a sym m (Exp a)
accessE l = liftR1 $ HM.accessE l

bind :: Monad m => Int -> Int -> T a sym m (Exp a)
bind i l = liftR0 $ HM.bind i l

undefined' :: Monad m => T a sym m (Exp a)
undefined' = liftR0 HM.undefined'

asTypeOf' :: Monad m => T a sym m (Exp a) -> T a sym m (Exp a) -> T a sym m (Exp a)
asTypeOf' = liftR2 HM.asTypeOf'

fix' :: Monad m => T a sym m (Exp a) -> T a sym m (Exp a)
fix' = liftR1 HM.fix'

return' :: Monad m => T a sym m (Exp a) -> T a sym m (Exp a)
return' = liftR1 HM.return'

then' :: Monad m => T a sym m (Exp a) -> T a sym m (Exp a) -> T a sym m (Exp a)
then' = liftR2 HM.then'

runCont :: Monad m => T a sym m (Exp a) -> T a sym m (Exp a)
runCont = liftR1 HM.runCont

callCC :: Monad m => T a sym m (Exp a) -> T a sym m (Exp a)
callCC = liftR1 HM.callCC

liftR0 :: Monad m => ReaderT r m a -> T r sym m a
liftR0 = withReaderT annotation

liftR1 :: Monad m => (ReaderT r m a -> ReaderT r m a) -> T r sym m a -> T r sym m a
liftR1 f m = do
  r <- ask
  liftR0 $ f (lowerR r m)

liftR2 :: Monad m =>
          (ReaderT r m a -> ReaderT r m a -> ReaderT r m a) ->
          T r sym m a -> T r sym m a -> T r sym m a
liftR2 f m n = do
  r <- ask
  liftR0 $ f (lowerR r m) (lowerR r n)

lowerR :: R r sym -> T r sym m a -> ReaderT r m a
lowerR r = withReaderT (\ a -> r { annotation = a })

type T a sym = ReaderT (R a sym)

initR :: a -> Ident -> Symtab sym -> R a sym
initR = R

data R a sym
  = R { annotation :: a
      , currentContinuation :: Ident
      , symbolTable :: Symtab sym
      }

localA :: MonadReader (R r sym) m => r -> m a -> m a
localA a = local (\ r -> r { annotation = a})

askCC :: MonadReader (R a sym) m => m Ident
askCC = asks currentContinuation

localCC :: MonadReader (R r sym) m => Ident -> m a -> m a
localCC cc = local (\ r -> r { currentContinuation = cc })

type Symtab sym = IdentMap (Record sym)

askSymtab :: MonadReader (R a sym) m => m (Symtab sym)
askSymtab = asks symbolTable

runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT
