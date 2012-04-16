{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , NoMonomorphismRestriction
  , OverloadedStrings
  , PatternGuards
  , Rank2Types
  , ScopedTypeVariables
  , TypeSynonymInstances #-}
module Language.Glyph.HM.InferType
       ( TypeEnvironment
       , inferType
       ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict hiding ((<>))

import Data.Foldable (toList)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.List (foldl')
import Data.Maybe
import Data.Semigroup
import Data.STRef
import Data.Typeable

import Language.Glyph.HM.Syntax
import Language.Glyph.Ident
import Language.Glyph.IdentSet (IdentSet, (\\), member)
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Msg hiding (singleton)
import qualified Language.Glyph.Msg as Msg
import Language.Glyph.Type hiding (Var)
import qualified Language.Glyph.Type as Type
import Language.Glyph.Unique

import Text.PrettyPrint.Free

import Prelude hiding (lookup, null)

import Debug.Trace

type Map = HashMap

inferType :: ( Pretty a
             , MonadError TypeException m
             , MonadWriter Msgs m
             , UniqueMonad m
             ) => Exp a -> m TypeEnvironment
inferType e = do
  ((psi, _c, _tau), gamma) <- runStateT (inferExp e) mempty
  return $! psi $$ gamma

type TypeEnvironment = Map Ident TypeScheme

newtype Substitution =
  Substitution { unSubstitution :: Map Type.Var Type
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
            , MonadState TypeEnvironment m
            , MonadWriter Msgs m
            , UniqueMonad m
            ) =>
            Exp a ->
            m (Substitution, Constraint Normal, Type)
inferExp = go
  where
    go (Exp a x) =
      runReaderT (w x) a
    w (VarE x) = do
      sigma <- lookup x
      (c, tau) <- instantiate sigma
      return (mempty, c, tau)
    w (AbsE x e) = do
      alpha <- fresh
      modify $ Map.insert x (mono alpha)
      (psi, c, tau) <- inferExp e
      return (psi $\ alpha, c, (psi $$ alpha) :->: tau)
    w (AppE e1 e2) = do
      (psi1, c1, tau1) <- inferExp e1
      (psi2, c2, tau2) <- localApply psi1 $ inferExp e2
      alpha <- fresh
      let d = Set.map toNonnormal ((psi2 $$ c1) <> c2) <>
              Set.singleton ((psi2 $$ tau1) := tau2 :->: alpha)
          psi' = psi2 $. psi1
      (c, psi) <- normalize d psi'
      gamma <- get
      return (psi $| typeVars gamma, c, psi $$ alpha)
    w (LetE x e e') = do
      (psi1, c1, tau) <- inferExp e
      gamma <- get
      (c2, sigma) <- generalize c1 (psi1 $$ gamma) tau
      modify $ Map.insert x sigma
      (psi2, c3, tau') <- localApply psi1 $ inferExp e'
      let d = (psi2 $$ c2) <> c3
          psi' = psi2 $. psi1
      (c, psi) <- normalize (Set.map toNonnormal d) psi'
      return (psi $| typeVars (Map.delete x gamma), c, psi $$ tau')
    w (LitE lit) =
      inferLit lit
    w (MkTuple x) = do
      alphas <- replicateM x fresh
      return (mempty, mempty, foldr (:->:) (Tuple alphas) alphas)
    w (Select x y) = do
      alphas <- replicateM y fresh
      return (mempty, mempty, Tuple alphas :->: (alphas !! x))
    w (Access l) = do
      a <- fresh
      b <- fresh
      return (mempty, Set.singleton (a `Has` (l, b)), a :->: b)
    w Undefined = do
      alpha <- fresh
      return (mempty, mempty, alpha)
    w AsTypeOf = do
      a <- fresh
      return (mempty, mempty, a :->: a :->: a)
    w Fix = do
      a <- fresh
      return (mempty, mempty, (a :->: a) :->: a)
    w RunCont = do
      a <- fresh
      return (mempty, mempty, Cont a :->: a)
    w Return = do
      a <- fresh
      return (mempty, mempty, a :->: Cont a)
    w Then = do
      a <- fresh
      b <- fresh
      return (mempty, mempty, Cont a :->: Cont b :->: Cont b)
    w CallCC = do
      a <- fresh
      b <- fresh
      return (mempty, mempty, ((a :->: Cont b) :->: Cont a) :->: Cont a)

inferLit :: Monad m =>
            Lit -> m (Substitution, Constraint Normal, Type)
inferLit x =
  return (mempty, mempty, tau)
  where
    tau =
      case x of
        BoolL _ -> Bool
        IntL _ -> Int
        DoubleL _ -> Double
        StringL _ -> String
        VoidL -> Void

type Normalize a s = ErrorT TypeException (ReaderT a (WriterT Msgs (ST s)))

normalize :: forall a m .
             ( Pretty a
             , MonadError TypeException m
             , MonadReader a m
             , MonadWriter Msgs m
             , UniqueMonad m
             ) =>
             Constraint Nonnormal ->
             Substitution ->
             m (Constraint Normal, Substitution)
normalize = runNormalize
  where
    runNormalize d phi =
      run (normalizeM' d phi)
    normalizeM' :: Constraint Nonnormal ->
                   Substitution ->
                   Normalize a s (Constraint Normal, Substitution)
    normalizeM' d phi = do
      d' <- newRef d
      phi' <- newRef phi
      normalizeM d' phi'
    normalizeM :: STRef s (Constraint Nonnormal) ->
                  STRef s Substitution ->
                  Normalize a s (Constraint Normal, Substitution)
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
          Type.Var a `Has` (l, tau) -> do
            d `forAll` (a, l) $ \ tau' ->
              modifyRef d $ \ d -> d `u` (tau := tau') \\ (Type.Var a `Has` (l, tau'))
            modifyRef c $ \ c -> c `u` (Type.Var a `Has` (l, tau))
          Int `Has` (l, tau) ->
            case Map.lookup l intLabels of
              Nothing -> do
                a <- ask
                tell $ Msg.singleton $ mkErrorMsg a $ MissingLabel Int l
              Just tau' ->
                modifyRef d $ \ d -> d `u` (tau := tau')
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
          Type.Var a' `Has` (l', tau') | a == a' && l == l' ->
            f tau'
          _ ->
            return ()
    run :: (forall s . Normalize a s (Constraint Normal, Substitution)) ->
           m (Constraint Normal, Substitution)
    run m = do
      r <- ask
      let (a, w) = runST (runWriterT (runReaderT (runErrorT m) r))
      tell w
      either throwError return a
    newRef = lift . lift . lift . newSTRef
    readRef = lift . lift . lift . readSTRef
    modifyRef r = lift . lift . lift . modifySTRef r
    writeRef r = lift . lift . lift . writeSTRef r
    u = flip Set.insert
    (\\) = flip Set.delete
    uncons xs =
      case toList xs of
        (x:_) -> Just (x, Set.delete x xs)
        [] -> Nothing

intLabels :: Map Label Type
intLabels =
  Map.fromList [ ("equals", Tuple [Int] :->: Bool)
               , ("plus", Tuple [Int] :->: Int)
               , ("minus", Tuple [Int] :->: Int)
               , ("toString", Tuple [] :->: String)
               ]

fresh :: UniqueMonad m => m Type
fresh = liftM Type.Var freshIdent

instantiate :: UniqueMonad m => TypeScheme -> m (Constraint Normal, Type)
instantiate (Forall alphas c tau) = do
  psi <- liftM (Substitution . Map.unions) $ forM alphas $ \ alpha -> do
    beta <- fresh
    return (Map.singleton alpha beta)
  return (psi $$ c, psi $$ tau)

generalize :: Monad m =>
              Constraint Normal ->
              TypeEnvironment ->
              Type ->
              m (Constraint Normal, TypeScheme)
generalize c gamma tau =
  return (c, poly alpha c tau)
  where
    alpha = toList $ (typeVars tau <> typeVars c) \\ typeVars gamma

lookup :: ( Pretty a
          , MonadError TypeException m
          , MonadReader a m
          , MonadState TypeEnvironment m
          , MonadWriter Msgs m
          , UniqueMonad m
          ) => Ident -> m TypeScheme
lookup x =
  get >>=
  maybe (throwError VarNotFound) return .
  Map.lookup x

deleteList :: (Eq k, Hashable k) => [k] -> Map k v -> Map k v
deleteList x gamma = foldl' (flip Map.delete) gamma x

localApply :: MonadState TypeEnvironment m => Substitution -> m a -> m a
localApply psi m = do
  s <- get
  put $! psi $$ s
  a <- m
  s' <- get
  put $! s `Map.union` s'
  return $! a

($\) :: Substitution -> Type -> Substitution
Substitution s $\ tau = Substitution $ deleteList alpha s
  where
    alpha = toList $ typeVars tau
infixl 4 $\ --

($|) :: Substitution -> IdentSet -> Substitution
Substitution s $| xs = Substitution $ Map.intersection s xs'
  where
    xs' = Map.fromList $ zip (toList xs) $ repeat ()
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

instance TypeVars (Constraint a) where
  typeVars = mconcat . map typeVars . toList

instance TypeVars (Predicate a) where
  typeVars = go
    where
      go (a := b) = typeVars a <> typeVars b
      go (a `Has` (_l, b)) = typeVars a <> typeVars b

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
        return $! Substitution $ Map.singleton x tau2
    (_, Type.Var _) ->
      mgu tau2 tau1
    (x :->: x', y :->: y') -> do
      psi1 <- mgu x y
      psi2 <- mgu (psi1 $$ x') (psi1 $$ y')
      return $! psi2 $. psi1
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
        return $! psi' $. psi
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

instance Apply Type where
  s $$ x =
    case x of
      Type.Var alpha -> fromMaybe x $ Map.lookup alpha (unSubstitution s)
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
  s $$ gamma = Map.map (s $$) gamma

instance Apply TypeScheme where
  Substitution s $$ Forall alpha c tau =
    Forall alpha (s' $$ c) (s' $$ tau)
    where
      s' = Substitution $ deleteList alpha s

instance Apply (Predicate a) where
  s $$ p = go p
    where
      go (a `Has` (l, b)) =
        (s $$ a) `Has` (l, s $$ b)
      go (tau1 := tau2) =
        (s $$ tau1) := (s $$ tau2)

instance Apply (Constraint a) where
  s $$ c = Set.map (s $$) c

instance Apply a => Apply [a] where
  s $$ xs = map (s $$) xs

($.) :: Substitution -> Substitution -> Substitution
s1 $. Substitution s2 = Substitution (Map.map (s1 $$) s2) <> s1
infixr 9 $.

mono :: Type -> TypeScheme

mono = Forall mempty mempty

poly :: [Type.Var] -> Constraint Normal -> Type -> TypeScheme
poly = Forall
