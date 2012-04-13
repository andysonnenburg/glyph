{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , NoMonomorphismRestriction
  , OverloadedStrings
  , Rank2Types
  , ScopedTypeVariables
  , TypeSynonymInstances #-}
module Language.Glyph.HM.InferType
       ( Substitution
       , inferType
       ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Writer.Strict hiding ((<>))

import Data.Foldable (toList)
import qualified Data.HashSet as Set
import Data.Maybe
import Data.Semigroup
import Data.STRef
import Data.Typeable

import Language.Glyph.HM.Syntax
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, delete, intersection, singleton)
import Language.Glyph.IdentSet (IdentSet, (\\), member)
import qualified Language.Glyph.IdentMap as IdentMap
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Msg hiding (singleton)
import qualified Language.Glyph.Msg as Msg
import Language.Glyph.Type
import qualified Language.Glyph.Type as Type
import Language.Glyph.Unique

import Text.PrettyPrint.Free

import Prelude hiding (lookup, null)

inferType :: ( Pretty a
             , MonadError TypeException m
             , MonadWriter Msgs m
             , UniqueMonad m
             ) => Exp a -> m (Substitution, Constraint, Type)
inferType = inferExp mempty

type TypeEnvironment = IdentMap TypeScheme

newtype Substitution =
  Substitution { unSubstitution :: IdentMap Type
               } deriving (Show, Semigroup, Monoid, NFData)

data TypeException
  = VarNotFound
  | TypeError Type Type
  | OccursCheckFailed Type Type
  | MissingLabel Type Label
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show TypeException where
  show = show . pretty

instance Pretty TypeException where
  pretty = go
    where
      go VarNotFound =
        text "var" <+>
        text "not" <+> 
        text "found"
      go (TypeError a b) =
        text "couldn't" <+>
        text "match" <+>
        text "type" <+> a' <+>
        text "and" <+> b'
        where
          (a', b') = prettyTypes (a, b)
      go (OccursCheckFailed a b) =
        text "occurs" <+>
        text "check" <+>
        text "failed" <+>
        text "for" <+> a' <+>
        text "and" <+> b'
        where
          (a', b') = prettyTypes (a, b)
      go (MissingLabel a l) =
        text "type" <+>
        pretty a <+>
        text "does" <+>
        text "not" <+>
        text "have" <+>
        text "method" <+>
        char '`' <> prettyLabel l <> char '\''
      go (StrMsgError s) =
        text s
      go NoMsgError =
        text "internal" <+>
        text "error"

instance Error TypeException where
  strMsg = StrMsgError
  noMsg = NoMsgError

instance Exception TypeException

inferExp :: ( Pretty a
            , MonadError TypeException m
            , MonadWriter Msgs m
            , UniqueMonad m
            ) =>
            TypeEnvironment ->
            Exp a ->
            m (Substitution, Constraint, Type)
inferExp = go
  where
    go gamma (Exp a x) =
      runReaderT (w gamma x) a
    w gamma (VarE x) = do
      sigma <- lookup x gamma
      (c, tau) <- instantiate sigma
      return (mempty, c, tau)
    w gamma (AbsE x e) = do
      alpha <- fresh
      (psi, c, tau) <- inferExp (delete x gamma <> singleton x (mono alpha)) e
      return (psi $\ alpha, c, (psi $$ alpha) :->: tau)
    w gamma (AppE e1 e2) = do
      (psi1, c1, tau1) <- inferExp gamma e1
      (psi2, c2, tau2) <- inferExp (psi1 $$ gamma) e2
      let psi' = psi2 $. psi1
      alpha <- fresh
      let d = (psi2 $$ c1) <> c2 <> Set.singleton ((psi2 $$ tau1) := tau2 :->: alpha)
      (c, psi) <- normalize d psi'
      return (psi, c, psi $$ alpha)
    w gamma (LetE x e e') = do
      (psi1, c1, tau) <- inferExp (delete x gamma) e
      (c2, sigma) <- generalize c1 (psi1 $$ gamma) tau
      (psi2, c3, tau') <- inferExp (delete x (psi1 $$ gamma) <> singleton x sigma) e'
      let d = (psi2 $$ c2) <> c3
          psi' = psi2 $. psi1
      (c, psi) <- normalize d psi'
      return (psi, c, psi $$ tau')
    w gamma (LitE lit) =
      inferLit gamma lit
    w _gamma (MkTuple x) = do
      alphas <- replicateM x fresh
      return (mempty, mempty, foldr (:->:) (Tuple alphas) alphas)
    w _gamma (Select x y) = do
      alphas <- replicateM y fresh
      return (mempty, mempty, Tuple alphas :->: (alphas !! x))
    w _gamma (Access l) = do
      a <- fresh
      b <- fresh
      return (mempty, Set.singleton (a :. (l, b)), a :->: b)
    w _gamma Undefined = do
      alpha <- fresh
      return (mempty, mempty, alpha)
    w _gamma AsTypeOf = do
      a <- fresh
      return (mempty, mempty, a :->: a :->: a)
    w _gamma Fix = do
      a <- fresh
      return (mempty, mempty, (a :->: a) :->: a)
    w _gamma RunCont = do
      a <- fresh
      return (mempty, mempty, Cont a :->: a)
    w _gamma Return = do
      a <- fresh
      return (mempty, mempty, a :->: Cont a)
    w _gamma Then = do
      a <- fresh
      b <- fresh
      return (mempty, mempty, Cont a :->: Cont b :->: Cont b)
    w _gamma CallCC = do
      a <- fresh
      b <- fresh
      return (mempty, mempty, ((a :->: Cont b) :->: Cont a) :->: Cont a)

inferLit :: Monad m =>
            TypeEnvironment -> Lit -> m (Substitution, Constraint, Type)
inferLit _gamma x =
  return (mempty, mempty, tau)
  where
    tau =
      case x of
        BoolL _ -> Bool
        IntL _ -> Int
        DoubleL _ -> Double
        StringL _ -> String
        VoidL -> Void

normalize :: forall a m .
             ( Pretty a
             , MonadReader a m
             , MonadWriter Msgs m
             , UniqueMonad m
             ) =>
             Constraint ->
             Substitution ->
             m (Constraint, Substitution)
normalize = runNormalize
  where
    runNormalize d phi =
      run (normalizeM' d phi)
    normalizeM' :: Constraint ->
                   Substitution ->
                   ReaderT a (WriterT Msgs (ST s)) (Constraint, Substitution)
    normalizeM' d phi = do
      d' <- newRef d
      phi' <- newRef phi
      normalizeM d' phi'
    normalizeM :: STRef s Constraint ->
                  STRef s Substitution ->
                  ReaderT a (WriterT Msgs (ST s)) (Constraint, Substitution)
    normalizeM d psi = do
      c <- newRef mempty
      whileJust (uncons <$> readRef d) $ \ (p, d') -> do
        writeRef d d'
        case p of
          tau := tau' -> do
            psi' <- mgu tau tau'
            modifyRef d (psi' $$)
            modifyRef c (psi' $$)
            modifyRef psi (psi' $.)
          Type.Var a :. (l, tau) -> do
            d `forAll` (a, l) $ \ tau' ->
              modifyRef d $ \ d -> d `u` (tau := tau') \\ (Type.Var a :. (l, tau'))
            modifyRef c $ \ c -> c `u` (Type.Var a :. (l, tau))
      c <- readRef c
      psi <- readRef psi
      return (c, psi)
    whileJust m f = go
      where
        go = do
          a <- m
          case a of
            Just j -> do
              _ <- f j
              go
            Nothing ->
              return ()
    (d `forAll` (a, l)) f = do
      d' <- readRef d
      forM_ (toList d') $ \ p ->
        case p of
          Var a' :. (l', tau') | a == a' && l == l' ->
            f tau'
          _ ->
            return ()
    run :: (forall s . ReaderT a (WriterT Msgs (ST s)) (Constraint, Substitution)) ->
           m (Constraint, Substitution)
    run m = do
      r <- ask
      let (a, w) = runST (runWriterT (runReaderT m r))
      tell w
      return a
    newRef = lift . lift . newSTRef
    readRef = lift . lift . readSTRef
    modifyRef r = lift . lift . modifySTRef r
    writeRef r = lift . lift . writeSTRef r
    u = flip Set.insert
    (\\) = flip Set.delete
    uncons xs =
      case toList xs of
        (x:_) -> Just (x, Set.delete x xs)
        [] -> Nothing

fresh :: UniqueMonad m => m Type
fresh = liftM Type.Var freshIdent

instantiate :: UniqueMonad m => TypeScheme -> m (Constraint, Type)
instantiate (Forall alphas c tau) = do
  psi <- liftM (Substitution . mconcat) $ forM alphas $ \ alpha -> do
    beta <- fresh
    return (IdentMap.singleton alpha beta)
  return (psi $$ c, psi $$ tau)

generalize :: Monad m =>
              Constraint ->
              TypeEnvironment ->
              Type ->
              m (Constraint, TypeScheme)
generalize c gamma tau =
  return (c, poly alpha c tau)
  where
    alpha = toList $ (typeVars tau <> typeVars c) \\ typeVars gamma

lookup :: ( Pretty a
          , MonadError TypeException m
          , MonadReader a m
          , MonadWriter Msgs m
          , UniqueMonad m
          ) => Ident -> IdentMap TypeScheme -> m TypeScheme
lookup x =
  maybe (throwError VarNotFound) return .
  IdentMap.lookup x

deleteList :: [Ident] -> IdentMap a -> IdentMap a
deleteList x gamma = foldr delete gamma x

($\) :: Substitution -> Type -> Substitution
Substitution s $\ tau = Substitution $ deleteList alpha s
  where
    alpha = toList $ typeVars tau
infixl 4 $\ --

($|) :: Substitution -> IdentSet -> Substitution
Substitution s $| xs = Substitution $ intersection s xs'
  where
    xs' = IdentMap.fromList $ zip (toList xs) $ repeat ()
infixl 4 $|

class TypeVars a where
  typeVars :: a -> IdentSet

instance TypeVars Type where
  typeVars tau =
    case tau of
      Type.Var alpha -> IdentSet.singleton alpha
      a :->: b -> typeVars a <> typeVars b
      Int -> mempty
      Double -> mempty
      Bool -> mempty
      String -> mempty
      Void -> mempty
      Record xs -> mconcat . map typeVars . toList $ xs
      Tuple xs -> mconcat . map typeVars $ xs
      Cont a -> typeVars a

instance TypeVars TypeEnvironment where
  typeVars = mconcat . map typeVars . toList

instance TypeVars TypeScheme where
  typeVars (Forall alpha c tau) =
    (typeVars c <> typeVars tau) \\ IdentSet.fromList alpha

instance TypeVars Constraint where
  typeVars = mconcat . map typeVars . toList

instance TypeVars Predicate where
  typeVars = go
    where
      go (a := b) = typeVars a <> typeVars b
      go (a :. (_l, b)) = typeVars a <> typeVars b

instance TypeVars a => TypeVars [a] where
  typeVars = mconcat . map typeVars

mgu :: ( Pretty a
       , MonadReader a m
       , MonadWriter Msgs m
       ) => Type -> Type -> m Substitution
mgu tau1 tau2 =
  case (tau1, tau2) of
    (Type.Var a, Type.Var b) | a == b ->
      return mempty
    (Type.Var x, _) ->
      if x `member` typeVars tau2
      then do
        a <- ask
        tell $ Msg.singleton $ mkErrorMsg a $ OccursCheckFailed tau1 tau2
        return mempty
      else
        return $ Substitution $ singleton x tau2
    (_, Type.Var _) ->
      mgu tau2 tau1
    (x :->: x', y :->: y') -> do
      psi1 <- mgu x y
      psi2 <- mgu (psi1 $$ x') (psi1 $$ y')
      return $ psi2 $. psi1
    (Int, Int) ->
      return mempty
    (Double, Double) ->
      return mempty
    (Bool, Bool) ->
      return mempty
    (Void, Void) ->
      return mempty
    (Tuple xs, Tuple ys) | length xs == length ys ->
      foldM' mempty (zip xs ys) $ \ psi (t, s) -> do
        psi' <- mgu (psi $$ t) (psi $$ s)
        return $ psi' $. psi
    (Cont tau1', Cont tau2') ->
      mgu tau1' tau2'
    _ -> do
      a <- ask
      tell $ Msg.singleton $ mkErrorMsg a $ TypeError tau1 tau2
      return mempty

foldM' :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
foldM' a bs f = foldM f a bs

class Apply a where
  ($$) :: Substitution -> a -> a
infixr 0 $$

instance Apply Substitution where
  s1 $$ Substitution s2 =
    Substitution $ fmap (s1 $$) s2

instance Apply Type where
  s $$ x =
    case x of
      Type.Var alpha -> fromMaybe x $ IdentMap.lookup alpha (unSubstitution s)
      a :->: b -> (s $$ a) :->: (s $$ b)
      Bool -> Bool
      Int -> Int
      Double -> Double
      String -> String
      Void -> Void
      Record xs -> Record . fmap (s $$) $ xs
      Tuple xs -> Tuple $ s $$ xs
      Cont a -> Cont $ s $$ a

instance Apply TypeEnvironment where
  s $$ gamma = (s $$) <$> gamma

instance Apply TypeScheme where
  Substitution s $$ Forall alpha c tau =
    Forall alpha (s' $$ c) (s' $$ tau)
    where
      s' = Substitution $ deleteList alpha s

instance Apply Predicate where
  s $$ p = go p
    where
      go (a :. (l, b)) =
        (s $$ a) :. (l, s $$ b)
      go (tau1 := tau2) =
        (s $$ tau1) := (s $$ tau2)

instance Apply Constraint where
  s $$ c = Set.map (s $$) c

instance Apply a => Apply [a] where
  s $$ xs = map (s $$) xs

($.) :: Substitution -> Substitution -> Substitution
s1 $. s2 = (s1 $$ s2) <> s1
infixr 9 $.

mono :: Type -> TypeScheme

mono = Forall mempty mempty

poly :: [Type.Var] -> Constraint -> Type -> TypeScheme
poly = Forall
