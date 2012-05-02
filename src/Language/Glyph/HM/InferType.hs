{-# LANGUAGE
    BangPatterns
  , DataKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , NoMonomorphismRestriction
  , OverloadedStrings
  , Rank2Types
  , ScopedTypeVariables
  , TypeSynonymInstances
  , ViewPatterns #-}
module Language.Glyph.HM.InferType
       ( TypeEnvironment
       , inferType
       ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad.Error hiding (forM, forM_)
import Control.Monad.Reader hiding (forM, forM_)
import Control.Monad.Ref hiding (forM, forM_)
import Control.Monad.ST

import Data.Foldable (foldr, forM_, toList)
import Data.Hashable
import Data.List (partition)
import Data.Maybe
import Data.Semigroup
import Data.STRef
import Data.Traversable (forM)
import Data.Typeable

import Language.Glyph.HM.Syntax
import Language.Glyph.Ident
import Language.Glyph.List.Strict (List, (!!))
import qualified Language.Glyph.List.Strict as List
import Language.Glyph.Map (Map)
import qualified Language.Glyph.Map as Map
import Language.Glyph.Msg hiding (singleton)
import qualified Language.Glyph.Msg as Msg
import Language.Glyph.Set (Set, member)
import qualified Language.Glyph.Set as Set
import Language.Glyph.State.Strict hiding (forM, forM_)
import Language.Glyph.Type hiding (Var)
import qualified Language.Glyph.Type as Type
import Language.Glyph.Unique
import Language.Glyph.Writer.Strict hiding ((<>), forM, forM_)

import Text.PrettyPrint.Free

import Prelude hiding ((!!), foldr, lookup)

import Unsafe.Coerce as Unsafe (unsafeCoerce)

inferType :: ( Pretty a
             , MonadError TypeException m
             , MonadWriter Msgs m
             , UniqueMonad m
             ) => Exp a -> m TypeEnvironment
inferType e = do
  (Inferred psi _c _tau, gamma) <- run $ inferExp e
  return $! psi $$ gamma
  where
    run = flip runStateT initS . flip runReaderT mempty

type S = TypeEnvironment

initS :: S
initS = mempty

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

data Inferred = Inferred !Substitution !(Constraint Normal) !Type

inferExp :: ( Pretty a
            , MonadError TypeException m
            , MonadReader TypeEnvironment m
            , MonadState S m
            , MonadWriter Msgs m
            , UniqueMonad m
            ) =>
            Exp a ->
            m Inferred
inferExp = go
  where
    go (Exp a x) =
      runReaderT (w x) a
    w (VarE x) = do
      sigma <- lift $ lookup x
      (c, tau) <- instantiate sigma
      return $! Inferred mempty c tau
    w (AbsE x e) = do
      alpha <- fresh
      Inferred psi c tau <- lift $ insert x (mono alpha) $ inferExp e
      return $! Inferred psi c ((psi $$ alpha) :->: tau)
    w (AppE e1 e2) = do
      Inferred psi1 c1 tau1 <- lift $ inferExp e1
      Inferred psi2 c2 tau2 <- lift $ local (psi1 $$) $ inferExp e2
      alpha <- fresh
      let d = Set.map toNonnormal ((psi2 $$ c1) <> c2) <>
              Set.singleton ((psi2 $$ tau1) := tau2 :->: alpha)
      Normalized c psi <- normalize d
      return $! Inferred (psi $. psi2 $. psi1) c (psi $$ alpha)
    w (LetE x e e') = do
      Inferred psi1 c1 tau <- lift $ inferExp e
      gamma <- lift ask
      let (c2, sigma) = generalize c1 (psi1 $$ gamma) tau
      Inferred psi2 c3 tau' <- lift $ local (psi1 $$) $ insert x sigma $ inferExp e'
      let d = (psi2 $$ c2) <> c3
      Normalized c psi <- normalize (Set.map toNonnormal d)
      return $! Inferred (psi $. psi2 $. psi1) c (psi $$ tau')
    w (LitE lit) =
      inferLit lit
    w (MkTuple x) = do
      alphas <- List.replicateM x fresh
      return $! Inferred mempty mempty (foldr (:->:) (Tuple alphas) alphas)
    w (Select i l) = do
      alphas <- List.replicateM l fresh
      return $! Inferred mempty mempty (Tuple alphas :->: (alphas !! i))
    w (MkRecord props) = do
      (props', c) <- runWriterT $ forM props $ \ x -> do
        sigma <- lift $ lift $ lookup x
        (c, tau) <- instantiate sigma
        tell $! c
        return $! tau
      return $! Inferred mempty c (Record props')
    w (Access l) = do
      a <- fresh
      b <- fresh
      return $! Inferred mempty (Set.singleton $ a `Has` (l, b)) (a :->: b)
    w (Bind i l) = do
      alpha <- List.replicateM l fresh
      beta <- fresh
      let tau1 = Tuple alpha :->: beta
          (alpha', alpha'') = List.splitAt i alpha
          tau2 = Tuple alpha'' :->: beta
      return $! Inferred mempty mempty (Tuple (tau1 `List.cons` alpha') :->: tau2)
    w Undefined = do
      alpha <- fresh
      return $! Inferred mempty mempty alpha
    w AsTypeOf = do
      a <- fresh
      return $! Inferred mempty mempty (a :->: a :->: a)
    w Fix = do
      a <- fresh
      return $! Inferred mempty mempty ((a :->: a) :->: a)
    w RunCont = do
      a <- fresh
      return $! Inferred mempty mempty (Cont a :->: a)
    w Return = do
      a <- fresh
      return $! Inferred mempty mempty (a :->: Cont a)
    w Then = do
      a <- fresh
      b <- fresh
      return $! Inferred mempty mempty (Cont a :->: Cont b :->: Cont b)
    w CallCC = do
      a <- fresh
      b <- fresh
      return $! Inferred mempty mempty (((a :->: Cont b) :->: Cont a) :->: Cont a)

inferLit :: Monad m =>
            Lit -> m Inferred
inferLit x =
  return $! Inferred mempty mempty tau
  where
    tau =
      case x of
        BoolL _ -> Bool
        IntL _ -> Int
        DoubleL _ -> Double
        StringL _ -> String
        VoidL -> Void

insert :: ( MonadReader TypeEnvironment m
          , MonadState S m
          ) =>
          Ident -> TypeScheme -> m a -> m a
insert x sigma m = do
  modify' f
  local f m
  where
    f = Map.insert x sigma

modify' :: MonadState s m => (s -> s) -> m ()
modify' f = do
  s <- get
  put $! f s

type Ref = STRef

data Normalized = Normalized !(Constraint Normal) !Substitution

normalize :: forall a m .
             ( Pretty a
             , MonadError TypeException m
             , MonadReader a m
             , MonadWriter Msgs m
             , UniqueMonad m
             ) =>
             Constraint Nonnormal ->
             m Normalized
normalize = runNormalize
  where
    runNormalize d =
      run (normalizeM' d mempty)
    normalizeM' d phi = do
      d' <- newRef d
      phi' <- newRef phi
      normalizeM d' phi'
    normalizeM d psi = do
      c <- newRef mempty
      whileJust (liftM uncons $ readRef d) $ \ (p, d') -> do
        writeRef d d'
        case p of
          tau := tau' -> do
            psi' <- mgu tau tau'
            modifyRef d (psi' $$)
            forRefM_ c $ \ p ->
              when (psi' `isDefinedAt` p) $ do
                modifyRef d (`u` toNonnormal (psi' $$ p))
                modifyRef c (\\ p)
            modifyRef psi (psi' $.)
          Type.Var a `Has` (l, tau) -> do
            d `forAll` (a, l) $ \ tau' ->
              modifyRef d $ \ d' -> d' `u` (tau := tau') \\ (Type.Var a `Has` (l, tau'))
            modifyRef c $ \ c' -> c' `u` (Type.Var a `Has` (l, tau))
          tau@(_ :->: _) `Has` (l, _) ->
            tellMissingLabel tau l
          Bool `Has` (l, tau) ->
            case Map.lookup l boolLabels of
              Nothing ->
                tellMissingLabel Bool l
              Just tau' ->
                modifyRef d (`u` (tau := tau'))
          Int `Has` (l, tau) ->
            case Map.lookup l intLabels of
              Nothing ->
                tellMissingLabel Int l
              Just tau' ->
                modifyRef d (`u` (tau := tau'))
          Double `Has` (l, tau) ->
            case Map.lookup l doubleLabels of
              Nothing ->
                tellMissingLabel Double l
              Just tau' ->
                modifyRef d (`u` (tau := tau'))
          Void `Has` (l, tau) ->
            case Map.lookup l voidLabels of
              Nothing ->
                tellMissingLabel Void l
              Just tau' ->
                modifyRef d (`u` (tau := tau'))
          tau@(Tuple _) `Has` (l, _) ->
            tellMissingLabel tau l
          tau@(Cont _) `Has` (l, _) ->
            tellMissingLabel tau l
      liftM2 Normalized (readRef c) (readRef psi)
    (d `forAll` (a, l)) f =
      forRefM_ d $ \ p ->
        case p of
          Type.Var a' `Has` (l', tau') | a == a' && l == l' ->
            f tau'
          _ ->
            return ()
    forRefM_ ref f = do
      x <- readRef ref
      forM_ x f
    run :: (forall s . RefSupplyT s m Normalized) -> m Normalized
    run m = runRefSupplyT m
    u = flip Set.insert
    (\\) = flip Set.delete
    uncons xs = go . toList $ xs
      where
        go (x:_) = Just (x, Set.delete x xs)
        go [] = Nothing

whileJust :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileJust m f = go
  where
    go = do
      a <- m
      case a of
        Nothing ->
          return ()
        Just j -> do
          f j
          go

tellMissingLabel :: ( Pretty a
                    , MonadReader a m
                    , MonadWriter Msgs m
                    ) => Type -> Label -> m ()
tellMissingLabel r l = do
  a <- ask
  tell $! Msg.singleton $ mkErrorMsg a $ MissingLabel r l

boolLabels :: Map Label Type
boolLabels =
  Map.fromList [ ("equals", Tuple (List.singleton Bool) :->: Bool)
               , ("hash", Tuple mempty :->: Int)
               , ("hashWithSalt", Tuple (List.singleton Int) :->: Int)
               , ("toString", Tuple mempty :->: String)
               ]

intLabels :: Map Label Type
intLabels =
  Map.fromList [ ("equals", Tuple (List.singleton Int) :->: Bool)
               , ("hash", Tuple mempty :->: Int)
               , ("hashWithSalt", Tuple (List.singleton Int) :->: Int)
               , ("plus", Tuple (List.singleton Int) :->: Int)
               , ("minus", Tuple (List.singleton Int) :->: Int)
               , ("toString", Tuple mempty :->: String)
               ]

doubleLabels :: Map Label Type
doubleLabels =
  Map.fromList [ ("equals", Tuple (List.singleton Double) :->: Bool)
               , ("hash", Tuple mempty :->: Int)
               , ("hashWithSalt", Tuple (List.singleton Int) :->: Int)
               , ("plus", Tuple (List.singleton Double) :->: Double)
               , ("minus", Tuple (List.singleton Double) :->: Double)
               , ("toString", Tuple mempty :->: String)
               ]

voidLabels :: Map Label Type
voidLabels =
  Map.fromList [ ("equals", Tuple (List.singleton Void) :->: Bool)
               , ("hash", Tuple mempty :->: Int)
               , ("hashWithSalt", Tuple (List.singleton Int) :->: Int)
               , ("toString", Tuple mempty :->: String)
               ]

fresh :: UniqueMonad m => m Type
fresh = liftM Type.Var freshVar

freshVar :: UniqueMonad m => m Type.Var
freshVar = freshIdent

instantiate :: UniqueMonad m => TypeScheme -> m (Constraint Normal, Type)
instantiate (Forall alpha c tau) = do
  psi <- liftM Substitution $ forM alpha' $ const fresh
  return (psi $$ c, psi $$ tau)
  where
    alpha' = setToMap alpha

generalize :: Constraint Normal ->
              TypeEnvironment ->
              Type ->
              (Constraint Normal, TypeScheme)
generalize c gamma tau =
  (d, poly alpha c' tau)
  where
    (Set.fromList -> d, Set.fromList -> c') =
      partition (\ p -> Set.null $ typeVars p `n` alpha) . toList $ c
    alpha = (typeVars tau <> typeVars c) \\ typeVars gamma
    n = Set.intersection
    (\\) = Set.difference

lookup :: ( MonadError TypeException m
          , MonadReader TypeEnvironment m
          , MonadWriter Msgs m
          , UniqueMonad m
          ) => Ident -> m TypeScheme
lookup x =
  ask >>=
  maybe (throwError VarNotFound) return .
  Map.lookup x

deleteSet :: (Eq k, Hashable k) => Set k -> Map k v -> Map k v
deleteSet x gamma = gamma `difference'` setToMap x

($\) :: Substitution -> Type -> Substitution
Substitution s $\ tau = Substitution $ deleteSet alpha s
  where
    alpha = typeVars tau
infixl 4 $\ --

($|) :: Substitution -> Set Type.Var -> Substitution
Substitution s $| xs = Substitution $ s `Map.intersection` setToMap xs
infixl 4 $|

class TypeVars a where
  typeVars :: a -> Set Type.Var

instance TypeVars Type where
  typeVars tau =
    case tau of
      Type.Var alpha -> Set.singleton alpha
      a :->: b -> typeVars a <> typeVars b
      Int -> mempty
      Double -> mempty
      Bool -> mempty
      String -> mempty
      Void -> mempty
      Record xs -> mconcat . map typeVars . toList $ xs
      Tuple xs -> mconcat . map typeVars . toList $ xs
      Cont a -> typeVars a

instance TypeVars TypeEnvironment where
  typeVars = mconcat . map typeVars . toList

instance TypeVars TypeScheme where
  typeVars (Forall alpha c tau) =
    (typeVars c <> typeVars tau) `Set.difference` alpha

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
        tell $! Msg.singleton $ mkErrorMsg a $ OccursCheckFailed tau1 tau2
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
    (Tuple xs, Tuple ys) | List.length xs == List.length ys ->
      foldM' mempty (List.zip xs ys) $ \ psi (t, s) -> do
        psi' <- mgu (psi $$ t) (psi $$ s)
        return $! psi' $. psi
    (Cont tau1', Cont tau2') ->
      mgu tau1' tau2'
    _ -> do
      tellTypeError tau1 tau2
      return mempty

tellTypeError :: ( Pretty a
                 , MonadReader a m
                 , MonadWriter Msgs m
                 ) => Type -> Type -> m ()
tellTypeError tau1 tau2 = do
  a <- ask
  tell $! Msg.singleton $ mkErrorMsg a $ TypeError tau1 tau2

foldM' :: Monad m => a -> List b -> (a -> b -> m a) -> m a
foldM' a bs f = List.foldM f a bs

class Apply a where
  ($$) :: Substitution -> a -> a
infixr 0 $$

instance Apply Type where
  (!s) $$ x =
    case x of
      Type.Var alpha -> Map.lookupDefault x alpha (unSubstitution s)
      (!a) :->: (!b) ->
        let !a' = s $$ a 
            !b' = s $$ b
        in a' :->: b'
      Bool -> Bool
      Int -> Int
      Double -> Double
      String -> String
      Void -> Void
      Record xs -> Record . fmap (s $$) $ xs
      Tuple !xs ->
        let !xs' = fmap (s $$) xs
        in Tuple xs'
      Cont !a ->
        let !a' = s $$ a
        in Cont a'

instance Apply TypeEnvironment where
  (!s) $$ gamma = Map.map (s $$) gamma

instance Apply TypeScheme where
  Substitution (!s) $$ Forall alpha (!c) (!tau) =
    let !c' = s' $$ c
        !tau' = s' $$ tau
    in Forall alpha c' tau'
    where
      s' = Substitution $ deleteSet alpha s

instance Apply (Predicate a) where
  (!s) $$ p = go p
    where
      go ((!a) `Has` (l, !b)) =
        let !a' = s $$ a
            !b' = s $$ b
        in a' `Has` (l, b')
      go ((!tau1) := (!tau2)) =
        let !tau1' = s $$ tau1
            !tau2' = s $$ tau2
        in tau1' := tau2'

instance Apply (Constraint a) where
  (!s) $$ c = Set.map (s $$) c

($.) :: Substitution -> Substitution -> Substitution
s1 $. Substitution s2 = Substitution (Map.map (s1 $$) s2) <> s1
infixr 9 $.

isDefinedAt :: (Apply a, TypeVars a) => Substitution -> a -> Bool
Substitution psi `isDefinedAt` a =
  not . Map.null $ psi `Map.intersection` setToMap (typeVars a)

mono :: Type -> TypeScheme

mono = Forall mempty mempty

poly :: Set Type.Var -> Constraint Normal -> Type -> TypeScheme
poly = Forall

difference' :: (Eq k, Hashable k) => Map k v -> Map k w -> Map k v
difference' = Map.foldlWithKey' f
  where
    f a k _ = Map.delete k a

setToMap :: Set a -> Map a ()
{-# INLINE setToMap #-}
setToMap = Unsafe.unsafeCoerce
