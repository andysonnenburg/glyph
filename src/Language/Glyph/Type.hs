{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , TypeSynonymInstances #-}
module Language.Glyph.Type
       ( TypeScheme (..)
       , Type (..)
       , Var
       , Label
       , Form (..)
       , Predicate (..)
       , toNonnormal
       , Constraint
       , Pretty
       , runPrettyType
       , PrettyTypeT
       , runPrettyTypeT
       , prettyM
       , prettyTypes
       , prettyLabel
       ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Identity hiding (mapM)
import Control.Monad.State.Strict hiding (get, mapM, put)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Writer hiding ((<>), mapM)

import Data.Data
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Traversable

import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.List.Strict (List)
import Language.Glyph.Pretty
import Language.Glyph.Stream
import Language.Glyph.Syntax (MethodName, prettyText)

import Prelude hiding (enumFrom, mapM)

type Map = HashMap
type Set = HashSet

type PrettyType e = PrettyTypeT e Identity

runPrettyType :: PrettyType e a -> a
runPrettyType = runIdentity . runPrettyTypeT

newtype PrettyTypeT e m a
  = PrettyTypeT { unPrettyTypeT :: StateT (Stream VarName, IdentMap (Doc e)) m a
                } deriving ( Functor
                           , Applicative
                           , Monad
                           , MonadTrans
                           , MonadIO
                           )

runPrettyTypeT :: Monad m => PrettyTypeT e m a -> m a
runPrettyTypeT = flip evalStateT (enumFrom (VarName 0), mempty) . unPrettyTypeT

class PrettyM a where
  prettyM :: Monad m => a -> PrettyTypeT e m (Doc e)

prettyDefault :: PrettyM a => a -> Doc e
prettyDefault = runPrettyType . prettyM

data TypeScheme = Forall (Set Var) (Constraint Normal) Type

instance Show TypeScheme where
  show = showDefault

instance Pretty TypeScheme where
  pretty = prettyDefault

instance PrettyM TypeScheme where
  prettyM (Forall alpha c tau) =
    if Set.null params
    then prettyM tau
    else do
      params' <- mapM prettyM . toList $ params
      tau' <- prettyM tau
      return $! parameters params' <+> tau'
    where
      c' = Set.fromList . concatParameters . map fromPredicate . toList $ c
      alpha' = Set.map fromVar alpha
      params = c' `Set.union` alpha'

parameters :: Foldable f => f (Doc e) -> Doc e
parameters = encloseSep (char '<') (char '>') (comma <> space)

data Parameter = HasP Type [(Label, Type)]

instance Eq Parameter where
  (Var a `HasP` _) == (Var a' `HasP` _) = a == a'
  (a `HasP` ps) == (a' `HasP` ps') = a == a' && ps == ps'

instance Hashable Parameter where
  hash (Var a `HasP` _) = 0 `hashWithSalt`
                          a
  hash (a `HasP` ps) = 1 `hashWithSalt`
                       a `hashWithSalt`
                       ps

fromPredicate :: Predicate Normal -> Parameter
fromPredicate (a `Has` p) = a `HasP` [p]

fromVar :: Var -> Parameter
fromVar = flip HasP mempty . Var

toParameter :: (Type, Set (Label, Type)) -> Parameter
toParameter (a, ps) = a `HasP` toList ps

concatParameters :: [Parameter] -> [Parameter]
concatParameters = map toParameter . Map.toList . foldl' f mempty
  where
    f m (tau `HasP` ps) =
      Map.insertWith mappend tau (Set.fromList ps) m

instance PrettyM Parameter where
  prettyM = go
    where
      go (a `HasP` ps) = do
        a' <- prettyM a
        if' (null ps) (return $! a') $ do
          ps' <- mapM prettyProp ps
          return $! a' <+> text "has" <+> props ps'
      if' True x _ = x
      if' False _ y = y
      props =
        encloseSep mempty mempty (space <> char '&' <> space)
      prettyProp (l, tau) = do
        tau' <- prettyM tau
        return $! prettyLabel l <> parameters [tau']

data Type
  = Var Var
  | Type :->: Type
  | Bool
  | Int
  | Double
  | String
  | Void
  | Record Record
  | Tuple (List Type)
  | Cont Type deriving (Eq, Typeable)
infixr 0 :->:

instance Hashable Type where
  hash (Var x) =
    0 `hashWithSalt`
    x
  hash (a :->: b) =
    1 `hashWithSalt`
    a `hashWithSalt`
    b
  hash Bool =
    2
  hash Int =
    3
  hash Double =
    4
  hash String =
    5
  hash Void =
    6
  hash (Record r) =
    7 `hashWithSalt`
    toList r
  hash (Tuple xs) =
    8 `hashWithSalt`
    xs
  hash (Cont a) =
    9 `hashWithSalt`
    a

instance Show Type where
  show = showDefault

instance Pretty Type where
  pretty = prettyDefault

instance PrettyM Type where
  prettyM tau =
    case tau of
      Var x ->
        prettyM x
      a :->: b -> do
        a' <- prettyM a
        b' <- prettyM b
        return $! text "fn" <> a' <> b'
      Bool ->
        return $! text "boolean"
      Int ->
        return $! text "int"
      Double ->
        return $! text "double"
      String ->
        return $! text "string"
      Void ->
        return $! text "void"
      Record r ->
        prettyM r
      Tuple xs -> do
        xs' <- mapM prettyM xs
        return $! tupled xs'
      Cont a -> do
        a' <- prettyM a
        return $! text "Cont#" <+> a'

instance NFData Type where
  rnf (Var x) = rnf x
  rnf (a :->: b) = rnf a `seq`
                   rnf b `seq`
                   ()
  rnf Bool = ()
  rnf Int = ()
  rnf Double = ()
  rnf String = ()
  rnf Void = ()
  rnf (Record r) = rnf r
  
  rnf (Tuple xs) = rnf xs
  rnf (Cont a) = rnf a

type Var = Ident

instance PrettyM Var where
  prettyM var = do
    (xs, m) <- get
    case IdentMap.lookup var m of
      Nothing -> do
        let (y :| ys) = xs
            doc = pretty y
        put (ys, IdentMap.insert var doc m)
        return doc
      Just doc ->
        return doc

get :: Monad m => PrettyTypeT e m (Stream VarName, IdentMap (Doc e))
get = PrettyTypeT State.get

put :: Monad m => (Stream VarName, IdentMap (Doc e)) -> PrettyTypeT e m ()
put = PrettyTypeT . State.put

newtype VarName = VarName Int deriving Enum

instance Pretty VarName where
  pretty (VarName x) =
    char (toEnum (r + a)) <> if q == 0 then mempty else pretty q
    where
      (q, r) = x `quotRem` size
      size = fromEnum 'Z' - a + 1
      a = fromEnum 'A'

type Record = Map Label Type

instance PrettyM Record where
  prettyM =
    liftM (encloseSep lbrace rbrace (comma <> space)) .
    mapM f .
    Map.toList
    where
      f (a, b) = do
        let a' = prettyLabel a
        b' <- prettyM b
        return $! a' <> colon <+> b'

type Label = MethodName

data Form
  = Normal
  | Nonnormal

data Predicate a where
  (:=) :: Type -> Type -> Predicate Nonnormal
  Has :: Type -> (Label, Type) -> Predicate a
infixr 0 :=

deriving instance Eq (Predicate a)

instance Show (Predicate a) where
  show = showDefault

instance Hashable (Predicate a) where
  hash (a := b) =
    0 `hashWithSalt`
    a `hashWithSalt`
    b
  hash (a `Has` p) =
    1 `hashWithSalt`
    a `hashWithSalt`
    p

instance Pretty (Predicate a) where
  pretty = prettyDefault

instance PrettyM (Predicate a) where
  prettyM = go
    where
      go (a := b) = do
        a' <- prettyM a
        b' <- prettyM b
        return $! a' <+> char '=' <+> b'
      go (a `Has` (l, b)) = do
        a' <- prettyM a
        b' <- prettyM b
        return $! a' <+> text "has" <+> prettyLabel l <> parameters [b']

instance NFData (Predicate a) where
  rnf (a := b) = rnf a `seq`
                 rnf b `seq`
                 ()
  rnf (a `Has` p) = rnf a `seq`
                    rnf p `seq`
                    ()

toNonnormal :: Predicate a -> Predicate Nonnormal
toNonnormal = go
  where
    go p@(_ := _) = p
    go (a `Has` x) = a `Has` x

type Constraint a = Set (Predicate a)

instance PrettyM (Constraint a) where
  prettyM = liftM tupled . mapM prettyM . toList

prettyTypes :: (Type, Type) -> (Doc e, Doc e)
prettyTypes = uncurry go
  where
    go a b = runPrettyType $ do
      a' <- prettyM a
      b' <- prettyM b
      return (a', b')

prettyLabel :: Label -> Doc e
prettyLabel = prettyText
