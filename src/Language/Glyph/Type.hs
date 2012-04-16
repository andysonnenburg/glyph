{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
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
       , prettyTypes
       , prettyLabel
       ) where

import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Writer hiding ((<>))

import Data.Data
import Data.Foldable
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashSet (HashSet)

import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Syntax (MethodName, prettyText)

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

type Set = HashSet

class Pretty' a where
  pretty' :: MonadState (Int, IdentMap (Doc e)) m => a -> m (Doc e)

prettyDefault :: Pretty' a => a -> Doc e
prettyDefault = evalState' . pretty'
  where
    evalState' =
      flip evalState (0, mempty)

showDefault :: Pretty a => a -> String
showDefault = show . pretty

data TypeScheme = Forall [Var] (Constraint Normal) Type

instance Pretty' TypeScheme where
  pretty' (Forall alpha c tau) = do
    alpha' <- mapM pretty' . toList $ alpha
    c' <- pretty' c
    tau' <- pretty' tau
    return $ hsep [text "forall", hsep alpha', char '.', c', text "=>", tau']

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

instance Pretty' Type where
  pretty' tau =
    case tau of
      Var x ->
        pretty' x
      a :->: b -> do
        a' <- pretty' a
        b' <- pretty' b
        return $ text "fn" <> a' <> b'
      Bool ->
        return $ text "boolean"
      Int ->
        return $ text "int"
      Double ->
        return $ text "double"
      String ->
        return $ text "string"
      Void ->
        return $ text "void"
      Record r ->
        pretty' r
      Tuple xs -> do
        xs' <- mapM pretty' xs
        return $ tupled xs'
      Cont a -> do
        a' <- pretty' a
        return $ text "Cont#" <+> a'

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

instance Pretty' Var where
  pretty' x = return $ pretty x {- do
    (a, m) <- get
    case IdentMap.lookup x m of
      Nothing -> do
        doc <- execWriterT $ do
          let (q, r) = a `quotRem` size
          tell $ char $ toEnum $ r + fromEnum 'A'
          unless (q == 0) $ tell $ pretty q
        put (a + 1, IdentMap.insert x doc m)
        return doc
      Just doc ->
        return doc
    where
      size = fromEnum 'Z' - fromEnum 'A' + 1 -}

type Record = Map Label Type

instance Pretty' Record where
  pretty' =
    liftM (encloseSep lbrace rbrace (comma <> space)) .
    mapM f .
    Map.toList
    where
      f (a, b) = do
        let a' = prettyLabel a
        b' <- pretty' b
        return $ a' <> colon <+> b'

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

instance Pretty' (Predicate a) where
  pretty' = go
    where
      go (a := b) = do
        a' <- pretty' a
        b' <- pretty' b
        return $ a' <+> char '=' <+> b'
      go (a `Has` (l, b)) = do
        a' <- pretty' a
        b' <- pretty' b
        return $ a' <+> text "has" <+> prettyLabel l <> colon <+> b'

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

instance Pretty' (Constraint a) where
  pretty' = liftM tupled . mapM pretty' . toList

prettyTypes :: (Type, Type) -> (Doc e, Doc e)
prettyTypes = uncurry go
  where
    go a b =
      evalState' $ do
        a' <- pretty' a
        b' <- pretty' b
        return (a', b')    
    evalState' =
      flip evalState (0, mempty)

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
