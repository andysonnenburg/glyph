{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
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

import Data.Maybe
import Data.Typeable

import Language.Glyph.HM.Syntax
import Language.Glyph.Ident
import Language.Glyph.IdentMap
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentMap as IdentMap
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Location
import Language.Glyph.Logger
import Language.Glyph.Message
import Language.Glyph.Monoid
import Language.Glyph.Type
import qualified Language.Glyph.Type as Type

inferType :: ( HasLocation a
            , MonadIdentSupply m
            , MonadLogger Message m
            ) => Exp a -> m (Substitution, Type)
inferType = inferExp mempty

type TypeEnvironment = IdentMap TypeScheme

newtype Substitution =
  Substitution { unSubstitution :: IdentMap Type
               } deriving (Show, Monoid)

data TypeException
  = TypeError Type Type
  | OccursCheckFailed Type Type
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show TypeException where
  show x =
    case x of
      TypeError a b ->
        let (a', b') = showTypes (a, b)
        in "couldn't match type `" ++ a' ++ "' and `" ++ b' ++ "'"
      OccursCheckFailed a b ->
        let (a', b') = showTypes (a, b)
        in "occurs check failed for `" ++ a' ++ "' and `" ++ b' ++ "'"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Error TypeException where
  strMsg = StrMsgError
  noMsg = NoMsgError

instance Exception TypeException

inferExp :: ( HasLocation a
           , MonadIdentSupply m
           , MonadLogger Message m
           ) => TypeEnvironment -> Exp a -> m (Substitution, Type)
inferExp = go
  where
    go gamma e =
      runReaderT (w gamma (view e)) (location e)
    w gamma (VarE x) = do
      let sigma = gamma!x
      tau <- instantiate sigma
      return (mempty, tau)
    w gamma (AbsE x e) = do
      (map (fmap mono) -> gamma', tau1) <- freshPat x
      (s1, tau2) <- inferExp (deletePat x gamma <> fromList gamma') e
      return (s1 $\ tau1, (s1 $$ tau1) :->: tau2)
    w gamma (AppE e1 e2) = do
      (s1, tau1) <- inferExp gamma e1
      (s2, tau2) <- inferExp (s1 $$ gamma) e2
      beta <- fresh
      s3 <- mgu (s2 $$ tau1) (tau2 :->: beta)
      return ((s3 $. s2 $. s1) $| typeVars gamma, s3 $$ beta)
    w gamma (LetE x e1 e2) = do
      (gamma', beta) <- freshPat x
      (s1, tau1) <- inferExp gamma e1
      s1' <- liftM ($. s1) $ mgu (s1 $$ beta) tau1
      gamma'' <- forM gamma' $ \ (x, tau) -> do
        sigma <- generalize (s1' $$ gamma) (s1' $$ tau)
        return (x, sigma)
      (s2, tau2) <- inferExp ((s1' $$ deletePat x gamma) <> fromList gamma'') e2
      return (s2 $. s1' $| typeVars gamma, s2 $$ tau2)
    w _gamma (BoolE _) =
      return (mempty, Bool)
    w _gamma VoidE =
      return (mempty, Void)
    w _gamma (IntE _) =
      return (mempty, Int)
    w _gamma (DoubleE _) =
      return (mempty, Double)
    w gamma (TupleE es) =
      tuple gamma es
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
    freshPat (VarP x) = do
      beta <- fresh
      return ([(x, beta)], beta)
    freshPat (TupleP xs) = do
      gamma <- forM xs $ \ x -> do
        beta <- fresh
        return (x, beta)
      return (gamma, Tuple $ map snd gamma)

tuple :: ( HasLocation a
        , MonadIdentSupply m
        , MonadLogger Message m
        ) => TypeEnvironment -> [Exp a] -> m (Substitution, Type)
tuple gamma es = do
  (s, reverse -> taus) <- go (reverse es)
  return (s, Tuple taus)
  where
    go [] =
      return (mempty, [])
    go (x:xs) = do
      (s1, taus) <- go xs
      (s2, tau) <- inferExp (s1 $$ gamma) x
      return (s2 $. s1, tau:taus)

fresh :: MonadIdentSupply m => m Type
fresh = liftM Type.Var newIdent

instantiate :: MonadIdentSupply m => TypeScheme -> m Type
instantiate (Forall alphas tau) = do
  (mconcat -> s) <- forM alphas $ \ alpha -> do
    beta <- fresh
    return $ Substitution $ singleton alpha beta
  return $ s $$ tau

generalize :: Monad m => TypeEnvironment -> Type -> m TypeScheme
generalize gamma tau = return $ poly alpha tau
  where
    alpha = IdentSet.toList $ typeVars tau \\ typeVars gamma

deletePat :: Pat -> TypeEnvironment -> TypeEnvironment
deletePat (VarP x) = delete x
deletePat (TupleP x) = deleteList x

deleteList :: [Ident] -> IdentMap a -> IdentMap a
deleteList x gamma = foldr delete gamma x

($\) :: Substitution -> Type -> Substitution
Substitution s $\ tau = Substitution $ deleteList alpha s
  where
    alpha = IdentSet.toList $ typeVars tau
infixl 4 $\

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

mgu :: ( MonadReader Location m
      , MonadLogger Message m
      ) => Type -> Type -> m Substitution
mgu tau1 tau2 =
  case (tau1, tau2) of
    (Type.Var a, Type.Var b) | a == b ->
      return mempty
    (Type.Var a, _) ->
      if IdentSet.member a (typeVars tau2)
        then do logError $ OccursCheckFailed tau1 tau2
                return mempty
        else return $ Substitution $ singleton a tau2
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
      logError $ TypeError tau1 tau2
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
      Int -> Int
      Double -> Double
      Bool -> Bool
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
