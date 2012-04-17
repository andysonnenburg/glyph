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
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer hiding ((<>))

import Data.Data
import Data.Foldable
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Syntax (MethodName, prettyText)

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

type Set = HashSet

type PrettyType e = PrettyTypeT e Identity

runPrettyType :: PrettyType e a -> a
runPrettyType = runIdentity . runPrettyTypeT

newtype PrettyTypeT e m a
  = PrettyTypeT { unPrettyTypeT :: StateT (Int, IdentMap (Doc e)) m a
                } deriving ( Functor
                           , Applicative
                           , Monad
                           , MonadTrans
                           , MonadIO
                           )

runPrettyTypeT :: Monad m => PrettyTypeT e m a -> m a
runPrettyTypeT = flip evalStateT (0, mempty) . unPrettyTypeT

class PrettyM a where
  prettyM :: Monad m => a -> PrettyTypeT e m (Doc e)

prettyDefault :: PrettyM a => a -> Doc e
prettyDefault = runPrettyType . prettyM

showDefault :: Pretty a => a -> String
showDefault = show . pretty

data TypeScheme = Forall [Var] (Constraint Normal) Type

instance PrettyM TypeScheme where
  prettyM (Forall alpha c tau) =
    if Set.null params
    then prettyM tau
    else do
      params' <- mapM prettyM . toList $ params
      tau' <- prettyM tau
      return $! parameters params' <+> tau'
    where
      c' = Set.map PredicateP $ c
      alpha' = Set.fromList . map VarP $ alpha
      params = c' `Set.union` alpha'

parameters :: Foldable f => f (Doc e) -> Doc e
parameters = encloseSep (char '<') (char '>') (comma <> space)

data Parameter
  = VarP Var
  | PredicateP (Predicate Normal)

instance Eq Parameter where
  VarP a == VarP b = a == b
  VarP a == PredicateP (Var b `Has` _) = a == b
  PredicateP (Var a `Has` _) == VarP b = a == b
  PredicateP a == PredicateP b = a == b
  _ == _ = False

instance Hashable Parameter where
  hash (VarP a) = 0 `hashWithSalt`
                  a
  hash (PredicateP (Var a `Has` _)) = 0 `hashWithSalt`
                                      a
  hash (PredicateP a) = 1 `hashWithSalt`
                        a

instance PrettyM Parameter where
  prettyM = go
    where
      go (VarP a) =
        prettyM a
      go (PredicateP a) =
        prettyM a

instance Pretty TypeScheme where
  pretty = prettyDefault

instance Show TypeScheme where
  show = showDefault

data Type
  = Var Var
  | Type :->: Type
  | Bool
  | Int
  | Double
  | String
  | Void
  | Record Record
  | Tuple [Type]
  | Cont Type deriving (Eq, Ord, Typeable)
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
  prettyM x = do
    (a, m) <- PrettyTypeT get
    case IdentMap.lookup x m of
      Nothing -> do
        doc <- execWriterT $ do
          let (q, r) = a `quotRem` size
          tell $ char $ toEnum $ r + fromEnum 'A'
          unless (q == 0) $ tell $ pretty q
        PrettyTypeT $ put (a + 1, IdentMap.insert x doc m)
        return doc
      Just doc ->
        return doc
    where
      size = fromEnum 'Z' - fromEnum 'A' + 1

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
deriving instance Ord (Predicate a)

instance Show (Predicate a) where
  show = showDefault

instance Hashable (Predicate a) where
  hash (a := b) =
    1 `hashWithSalt`
    a `hashWithSalt`
    b
  hash (a `Has` (l, b)) =
    0 `hashWithSalt`
    a `hashWithSalt`
    l `hashWithSalt`
    b

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
  rnf (a `Has` (l, b)) = rnf a `seq`
                         rnf l `seq`
                         rnf b `seq`
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

tupled :: Foldable f => f (Doc e) -> Doc e
tupled = encloseSep lparen rparen (comma <> space)

encloseSep :: Foldable f => Doc e -> Doc e -> Doc e -> f (Doc e) -> Doc e
encloseSep left right sp ds0 =
  case toList ds0 of
    [] -> left <> right
    [d] -> left <> d <> right
    ds -> left <> align (hcat (zipWith (<>) (init ds) (repeat sp) ++ [last ds <> right]))
