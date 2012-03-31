{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , GADTs
  , RankNTypes
  , ScopedTypeVariables
  , ViewPatterns #-}
module Language.Glyph.IR.ToHM
       ( toHM
       ) where

import Compiler.Hoopl
import Control.Monad.Reader

import Data.Graph (flattenSCC, stronglyConnCompR)
import Data.Monoid

import Language.Glyph.Hoopl
import Language.Glyph.HM.Syntax (Exp)
import qualified Language.Glyph.HM.Syntax as HM
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import Language.Glyph.IdentSet (IdentSet)
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.IR.Syntax
import Language.Glyph.Record hiding (Symtab, select)
import qualified Language.Glyph.Record as Record
import Language.Glyph.Unique ()

toHM :: ( Monoid a 
        , Select CallSet IdentSet sym
        , Select Record.Symtab (Symtab sym) fields
        , Select Insns (Graph (Insn a) O C) fields
        , UniqueMonad m
        ) => Record fields -> m (Exp a)
toHM r = toExp (r#.symtab) (r#.insns)

toList :: Graph n e x -> [SomeNode n]
toList = foldGraphNodesR f []
  where
    f x xs = SomeNode x : xs

data SomeNode n = forall e x . SomeNode (n e x)

type SomeInsn a = SomeNode (Insn a)

toExp :: ( Monoid a
         , Select CallSet IdentSet sym
         , UniqueMonad m
         ) => Symtab sym -> Graph (Insn a) O C -> m (Exp a)
toExp m (toList -> xs) = do
  cc <- freshIdent
  let r = initR (foldr f mempty xs) cc m
  runReaderT' r $ runCont $ callCC $
    absE cc (insnsToExp xs)
    where
      f (SomeNode (Stmt a _)) = mappend a
      f (SomeNode (Expr a _ _ _)) = mappend a
      f _ = id

funToExp :: ( Select CallSet IdentSet sym
            , UniqueMonad m
            ) => [Ident] -> Graph (Insn a) O C -> T a sym m (Exp a)
funToExp ps (toList -> xs) = do
  x <- freshIdent
  absE x $
    selectList x ps $
    runCont $ callCC $ do
      cc <- freshIdent
      absE cc (localCC cc $ insnsToExp xs)

insnsToExp :: ( Select CallSet IdentSet sym
              , UniqueMonad m
              ) => [SomeInsn a] -> T a sym m (Exp a)
insnsToExp xs = vars (funs (mapM_' go xs))
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

    vars = concatMap' var xs
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
              appE (absE x (localA a e)) undefined'

    funs =
      foldr' let' .
      map (toLet . flattenSCC) .
      stronglyConnCompR <$> callGraph xs
      where
        foldr' f as b = foldr f b as
        infixl 4 <$>
        (f <$> a) b = do
          a' <- a
          f a' b
        toLet xs' = (map snd' xs', map fst' xs')
        fst' (x, _, _) = x
        snd' (_, x, _) = x
        let' (xs', e1) e2 = do
          x <- freshIdent
          y <- freshIdent
          letE x (fix' (absE y $ selectList y xs' $ tupleE e1)) $
            selectList x xs' e2

callGraph :: forall a sym m .
             (Select CallSet IdentSet sym, UniqueMonad m) =>
             [SomeInsn a] -> T a sym m [(T a sym m (Exp a), Ident, [Ident])]
callGraph = concatMap' go'
  where
    concatMap' f = liftM mconcat . mapM f
    go' (SomeNode i) = go i
    go :: forall e x . Insn a e x -> T a sym m [(T a sym m (Exp a), Ident, [Ident])]
    go (Stmt a (FunDeclS (ident -> x) (map ident -> params) graph)) =
      fun a x params graph
    go (Expr a _ (FunE x (map ident -> params) graph) _) =
      fun a x params graph
    go _ =
      return mempty
    fun :: a -> Ident -> [Ident] -> Graph (Insn a) O C ->
           T a sym m [(T a sym m (Exp a), Ident, [Ident])]
    fun a x params graph = do
      m <- askSymtab
      let xs = IdentSet.toList $ (m ! x)#.callSet
      return [(e, x, xs)]
        where
          e = localA a $ funToExp params graph

stmtToExp :: Monad m => Stmt a x -> T a sym m (Exp a)
stmtToExp = go
  where
    go (ExprS x _) =
      return' $ varE x
    go (VarDeclS (ident -> x)) =
      return' $ varE x
    go (FunDeclS {}) =
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

exprToExp :: Monad m => Expr a -> T a sym m (Exp a)
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
          (forall e x . Insn a e x -> T a sym m (Exp a)) ->
          [SomeInsn a] ->
          T a sym m (Exp a)
mapM_' f as = sequence_' (map f' as)
  where
    f' (SomeNode i) = f i

sequence_' :: Monad m => [T a sym m (Exp a)] -> T a sym m (Exp a)
sequence_' = go
  where
    go [] = return' undefined'
    go (x:xs) = foldl then' x xs

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
    go ys e = snd (foldr f (0, id) ys) e
      where
        f y (i, f') = (i + 1, letE y (appE (select i l) (varE x)) . f')
        l = length ys

select :: Monad m => Int -> Int -> T a sym m (Exp a)
select i l = liftR0 $ HM.select i l

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

askA :: MonadReader (R a sym) m => m a
askA = asks annotation

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
