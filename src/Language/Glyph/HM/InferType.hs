{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , ViewPatterns #-}
module Language.Glyph.HM.InferType
       ( Substitution
       , inferType
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Maybe

import Language.Glyph.Generics
import Language.Glyph.HM.Syntax
import Language.Glyph.Ident
import Language.Glyph.IdentMap
import Language.Glyph.IdentSet (IdentSet, (\\))
import qualified Language.Glyph.IdentMap as IdentMap
import qualified Language.Glyph.IdentSet as IdentSet
import Language.Glyph.Location
import Language.Glyph.Message
import Language.Glyph.Monoid
import Language.Glyph.Type
import qualified Language.Glyph.Type as Type

inferType :: ( HasLocation a
            , MonadIdentSupply m
            , MonadWriter Message m
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
        "couldn't match type `" ++ show a ++ "' and `" ++ show b ++ "'"
      OccursCheckFailed a b ->
        "occurs check failed for `" ++ show a ++ "' and `" ++ show b ++ "'"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Error TypeException where
  strMsg = StrMsgError
  noMsg = NoMsgError

instance Exception TypeException

inferExp :: ( HasLocation a
           , MonadIdentSupply m
           , MonadWriter Message m
           ) => TypeEnvironment -> Exp a -> m (Substitution, Type)
inferExp = go
  where
    go gamma e =
      runReaderT (w gamma (view e)) (location e)
    w gamma (VarE x) = do
      let sigma = gamma!x
      tau <- instantiate sigma
      return (mempty, tau)
    w gamma (AbsE p e) = do
      (gamma', s1, tau1) <- pat p
      (s2, tau2) <- inferExp (apply s1 (deletePat p gamma) <> gamma') e
      return (s2 `compose` s1, apply s2 tau1 :->: tau2)
    w gamma (AppE e1 e2) = do
      (s1, tau1) <- inferExp gamma e1
      (s2, tau2) <- inferExp (apply s1 gamma) e2
      beta <- fresh
      s3 <- mgu (apply s2 tau1) (tau2 :->: beta)
      return (s3 `compose` s2 `compose` s1, apply s3 beta)
    w gamma (LetE x e1 e2) = do
      beta <- replicateM (length x) fresh
      (s1, tau1) <- inferExp gamma e1
      s1' <- mgu (apply s1 (Tuple beta)) tau1
      sigma <- mapM (generalize (apply s1' gamma) . apply s1') beta
      (s2, tau2) <- inferExp (apply s1' (deleteList x gamma) <> fromLists x sigma) e2
      return (s2 `compose` s1' `compose` s1, apply s2 tau2)
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
      return (mempty, a :->: (a :->: a))
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
      return (mempty, Cont a :->: (Cont b :->: Cont b))
    w _gamma CallCC = do
      a <- fresh
      b <- fresh
      return (mempty, ((a :->: Cont b) :->: Cont a) :->: Cont a)
    pat (VarP x) = do
      beta <- fresh
      return (singleton x (mono beta), mempty, beta)
    pat (TupleP x) = do
      beta <- replicateM (length x) fresh
      return (fromLists x (map mono beta), mempty, Tuple beta)

tuple :: ( HasLocation a
        , MonadIdentSupply m
        , MonadWriter Message m
        ) => TypeEnvironment -> [Exp a] -> m (Substitution, Type)
tuple gamma es = do
  (s, reverse -> taus) <- go (reverse es)
  return (s, Tuple taus)
  where
    go [] =
      return (mempty, [])
    go (x:xs) = do
      (s1, taus) <- go xs
      (s2, tau) <- inferExp (apply s1 gamma) x
      return (s2 `compose` s1, tau:taus)

fresh :: MonadIdentSupply m => m Type
fresh = liftM Type.Var newIdent

instantiate :: MonadIdentSupply m => TypeScheme -> m Type
instantiate (Forall alpha tau) = do
  (mconcat -> s) <- forM alpha $ \ alpha -> do
    beta <- fresh
    return $ Substitution $ singleton alpha beta
  return $ apply s tau

generalize :: Monad m => TypeEnvironment -> Type -> m TypeScheme
generalize gamma tau = return $ poly alpha tau
  where
    alpha = IdentSet.toList $ typeVars tau \\ typeVars gamma

deletePat :: Pat -> TypeEnvironment -> TypeEnvironment
deletePat (VarP x) = delete x
deletePat (TupleP x) = deleteList x

deleteList :: [Ident] -> TypeEnvironment -> TypeEnvironment
deleteList x gamma = foldr delete gamma x

fromLists :: [Ident] -> [TypeScheme] -> TypeEnvironment
fromLists k v = fromList $ zip k v

typeVars :: (Show a, Data a) => a -> IdentSet
typeVars =
  everythingBut (<>)
  ((mempty, False) `mkQ`
   queryTypeScheme `extQ`
   queryType)
  where
    queryTypeScheme (Forall alpha tau) =
      (typeVars tau \\ IdentSet.fromList alpha, True)
    
    queryType (Type.Var alpha) =
      (IdentSet.singleton alpha, False)
    queryType _ =
      (mempty, False)

{-
normalize :: MonadWriter Message m => Constraints -> Subst -> m (Constraints, Subst)
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
      , MonadWriter Message m
      ) => Type -> Type -> m Substitution
mgu tau tau' =
  case (tau, tau') of
    (Type.Var a, Type.Var b) | a == b ->
      return mempty
    (Type.Var a, _) ->
      if IdentSet.member a (typeVars tau')
        then do tellError $ OccursCheckFailed tau tau'
                return mempty
        else return $ Substitution $ singleton a tau'
    (_, Type.Var _) ->
      mgu tau' tau
    (x :->: x', y :->: y') -> do
      psi1 <- mgu x y
      psi2 <- mgu (apply psi1 x') (apply psi1 y')
      return $ psi2 `compose` psi1
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
            psi' <- mgu (apply psi t) (apply psi s)
            return $ psi' `compose` psi
      in foldM f mempty (zip xs ys)
    (Cont tau, Cont tau') ->
      mgu tau tau'
    _ -> do
      tellError $ TypeError tau tau'
      return mempty

apply :: Data a => Substitution -> a -> a
apply = go
  where
    go s x = runReader (transform x) s
    
    transform :: MonadReader Substitution m => GenericM m
    transform =
      everywhereButM' (mkM' transformTypeScheme `extM'` transformType)
    
    transformTypeScheme (Forall alpha tau) = do
      let f = Substitution . flip difference alpha' . unSubstitution
      tau' <- local f $ transform tau
      return (Forall alpha tau', True)
      where
        alpha' = fromList $ zip alpha (repeat ())
    
    transformType x@(Type.Var alpha) = do
      tau <- asks $ fromMaybe x . IdentMap.lookup alpha . unSubstitution
      return (tau, False)
    transformType x =
      return (x, False)

compose :: Substitution -> Substitution -> Substitution
s1 `compose` s2 = Substitution (apply s1 <$> unSubstitution s2) <> s1

mono :: Type -> TypeScheme
mono = Forall mempty

poly :: [Type.Var] -> Type -> TypeScheme
poly = Forall
