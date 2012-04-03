{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , TypeSynonymInstances
  , ViewPatterns #-}
module Language.Glyph.HM.InferType
       ( Substitution
       , inferType
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer hiding ((<>))

import Data.Maybe
import Data.Semigroup
import Data.Typeable

import Language.Glyph.HM.Syntax
import Language.Glyph.Ident
import Language.Glyph.IdentMap hiding ((\\), foldr, lookup, map)
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentMap as IdentMap
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Msg hiding (singleton)
import qualified Language.Glyph.Msg as Msg
import Language.Glyph.Type
import qualified Language.Glyph.Type as Type
import Language.Glyph.Unique

import Text.PrettyPrint.Free

import Prelude hiding (lookup)

inferType :: ( Pretty a
             , MonadError TypeException m
             , MonadWriter Msgs m
             , UniqueMonad m
             ) => Exp a -> m (Substitution, Type)
inferType = inferExp mempty

type TypeEnvironment = IdentMap TypeScheme

newtype Substitution =
  Substitution { unSubstitution :: IdentMap Type
               } deriving (Show, Semigroup, Monoid)

data TypeException
  = VarNotFound
  | TypeError Type Type
  | OccursCheckFailed Type Type
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
            ) => TypeEnvironment -> Exp a -> m (Substitution, Type)
inferExp = go
  where
    go gamma (Exp a x) =
      runReaderT (w gamma x) a
    w gamma (VarE x) = do
      sigma <- lookup x gamma
      tau <- instantiate sigma
      return (mempty, tau)
    w gamma (AbsE x e) = do
      beta <- fresh
      (s1, tau1) <- inferExp (delete x gamma <> singleton x (mono beta)) e
      return (s1, (s1 $$ beta) :->: tau1)
    w gamma (AppE e1 e2) = do
      (s1, tau1) <- inferExp gamma e1
      (s2, tau2) <- inferExp (s1 $$ gamma) e2
      beta <- fresh
      s3 <- mgu (s2 $$ tau1) (tau2 :->: beta)
      return (s3 $. s2 $. s1, s3 $$ beta)
    w gamma (LetE x e1 e2) = do
      (s1, tau1) <- inferExp gamma e1
      sigma <- generalize (s1 $$ gamma) tau1
      (s2, tau2) <- inferExp ((s1 $$ delete x gamma) <> singleton x sigma) e2
      return (s2 $. s1, tau2)
    w gamma (LitE lit) =
      inferLit gamma lit
    w _gamma (MkTuple x) = do
      alphas <- replicateM x fresh
      return (mempty, foldr (:->:) (Tuple alphas) alphas)
    w _gamma (Select x y) = do
      alphas <- replicateM y fresh
      return (mempty, Tuple alphas :->: (alphas !! x))
    w _gamma Undefined = do
      alpha <- fresh
      return (mempty, alpha)
    w _gamma AsTypeOf = do
      a <- fresh
      return (mempty, a :->: a :->: a)
    w _gamma Fix = do
      a <- fresh
      return (mempty, (a :->: a) :->: a)
    w _gamma RunCont = do
      a <- fresh
      return (mempty, Cont a :->: a)
    w _gamma Return = do
      a <- fresh
      return (mempty, a :->: Cont a)
    w _gamma Then = do
      a <- fresh
      b <- fresh
      return (mempty, Cont a :->: Cont b :->: Cont b)
    w _gamma CallCC = do
      a <- fresh
      b <- fresh
      return (mempty, ((a :->: Cont b) :->: Cont a) :->: Cont a)

inferLit :: Monad m => TypeEnvironment -> Lit -> m (Substitution, Type)
inferLit _gamma x =
  return
  (mempty,
   case x of
     BoolL _ -> Bool
     IntL _ -> Int
     DoubleL _ -> Double
     StringL _ -> String
     VoidL -> Void)

fresh :: UniqueMonad m => m Type
fresh = liftM Type.Var freshIdent

instantiate :: UniqueMonad m => TypeScheme -> m Type
instantiate (Forall alphas tau) = do
  (mconcat -> s) <- forM alphas $ \ alpha -> do
    beta <- fresh
    return $ Substitution $ singleton alpha beta
  return $ s $$ tau

generalize :: Monad m => TypeEnvironment -> Type -> m TypeScheme
generalize gamma tau = return $ poly alpha tau
  where
    alpha = IdentSet.toList $ typeVars tau \\ typeVars gamma

lookup :: ( Pretty a
          , MonadError TypeException m
          , MonadReader a m
          , MonadWriter Msgs m
          , UniqueMonad m
          ) => Ident -> IdentMap TypeScheme -> m TypeScheme
lookup x gamma =
  case IdentMap.lookup x gamma of
    Nothing ->
      throwError VarNotFound
    Just sigma ->
      return sigma

deleteList :: [Ident] -> IdentMap a -> IdentMap a
deleteList x gamma = foldr delete gamma x

($\) :: Substitution -> Type -> Substitution
Substitution s $\ tau = Substitution $ deleteList alpha s
  where
    alpha = IdentSet.toList $ typeVars tau
infixl 4 $\ --

($|) :: Substitution -> IdentSet -> Substitution
Substitution s $| xs = Substitution $ intersection s xs'
  where
    xs' = IdentMap.fromList $ zip (IdentSet.toList xs) $ repeat ()
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
      Void -> mempty
      Tuple xs -> mconcat . map typeVars $ xs
      Cont a -> typeVars a

instance TypeVars TypeEnvironment where
  typeVars = mconcat . map typeVars . IdentMap.elems

instance TypeVars TypeScheme where
  typeVars (Forall alpha tau) = typeVars tau \\ IdentSet.fromList alpha

{-
normalize :: MonadLogger Message m => Constraints -> Subst -> m (Constraints, Subst)
normalize = go
  where
    go d phi = evalStateT normalize' (d, c, psi)
      where
        c = mempty
        psi = phi

    getD = gets $ \ (d, _, _) -> d
    putD d = modify $ \ (_, c, psi) -> (d, c, psi)
    modifyD f = modify $ \ (d, c, psi) -> (f d, c, psi)
    getC = gets $ \ (_, c, _) -> c
    getPsi = gets $ \ (_, _, psi) -> psi
    modifyPsi f = modify $ \ (d, c, psi) -> (d, c, f psi)

    normalize' = do
      whileJust_ (liftM uncons getD) $ \ (p, d) -> do
        putD d
        case p of
          tau :==: tau' -> do
            psi <- getPsi
            psi' <- mgu (apply psi tau) (apply psi tau')
            modifyD $ apply psi'
            modifyPsi $ flip compose psi'
      c <- getC
      psi <- getPsi
      return (c, psi)

whileJust_ :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whileJust_ p f = go
  where
    go = do
      x <- p
      maybe nothing just x
      where
        nothing =
          return ()
        just x = do
          _ <- f x
          go

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)
-}

mgu :: ( Pretty a
       , MonadReader a m
       , MonadWriter Msgs m
       ) => Type -> Type -> m Substitution
mgu tau1 tau2 =
  case (tau1, tau2) of
    (Type.Var a, Type.Var b) | a == b ->
      return mempty
    (Type.Var x, _) ->
      if IdentSet.member x (typeVars tau2)
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
      let f psi (t, s) = do
            psi' <- mgu (psi $$ t) (psi $$ s)
            return $ psi' $. psi
      in foldM f mempty (zip xs ys)
    (Cont tau1', Cont tau2') ->
      mgu tau1' tau2'
    _ -> do
      a <- ask
      tell $ Msg.singleton $ mkErrorMsg a $ TypeError tau1 tau2
      return mempty

class Apply a where
  ($$) :: Substitution -> a -> a
infixr 0 $$

instance Apply Substitution where
  s1 $$ Substitution s2 = Substitution $ (s1 $$) <$> s2

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
      Tuple xs -> Tuple $ s $$ xs
      Cont a -> Cont $ s $$ a

instance Apply TypeEnvironment where
  s $$ gamma = (s $$) <$> gamma

instance Apply TypeScheme where
  Substitution s $$ Forall alpha tau = Forall alpha $ s' $$ tau
    where
      s' = Substitution $ deleteList alpha s

instance Apply a => Apply [a] where
  s $$ xs = map (s $$) xs

($.) :: Substitution -> Substitution -> Substitution
s1 $. s2 = (s1 $$ s2) <> s1
infixr 9 $.

mono :: Type -> TypeScheme
mono = Forall mempty

poly :: [Type.Var] -> Type -> TypeScheme
poly = Forall
