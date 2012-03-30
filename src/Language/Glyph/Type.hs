{-# LANGUAGE DeriveDataTypeable #-}
module Language.Glyph.Type
       ( TypeScheme (..)
       , Type (..)
       , Label
       , Var
       , prettyTypes
       ) where

import Control.Monad.State
import Control.Monad.Writer hiding ((<>))

import Data.Data
import Data.Foldable
import Data.Map (Map)

import Language.Glyph.Ident.Internal
import qualified Language.Glyph.IdentMap as IdentMap

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

data TypeScheme = Forall [Var] Type deriving (Show, Typeable, Data)

data Type
  = Var Var
  | Record (Map Label Type)
  | Type :->: Type
  | Int
  | Double
  | Bool
  | Void

  | Tuple [Type]
  | Cont Type deriving (Show, Typeable, Data)

infixr 9 :->:

prettyTypes :: (Type, Type) -> (Doc e, Doc e)
prettyTypes = uncurry go
  where
    go a b =
      evalState' $ do
        a' <- prettyTypeM a
        b' <- prettyTypeM b
        return (a', b')
    
    evalState' = flip evalState (0, IdentMap.empty)
    
    prettyTypeM tau =
      case tau of
        Var x ->
          prettyVarM x
        a :->: b -> do
          a' <- prettyTypeM a
          b' <- prettyTypeM b
          return $ a' <+> text "->" <+> b'
        Int ->
          return $ text "int"
        Double ->
          return $ text "double"
        Bool ->
          return $ text "bool"
        Void ->
          return $ text "void"
        Tuple xs -> do
          xs' <- mapM prettyTypeM xs
          return $ tupled xs'
        Cont a -> do
          a' <- prettyTypeM a
          return $ text "Cont#" <+> a'
    prettyVarM x = do
      (a, m) <- get
      case IdentMap.lookup x m of
        Nothing -> do
          doc <- execWriterT $ do
            let (q, r) = a `quotRem` size
            tell $ char '\''
            tell $ char $ toEnum $ r + fromEnum 'a'
            unless (q == 0) $ tell $ pretty q
          put (a + 1, IdentMap.insert x doc m)
          return doc
        Just doc ->
          return doc
      where
        size = fromEnum 'z' - fromEnum 'a' + 1

tupled :: Foldable f => f (Doc e) -> Doc e
tupled = encloseSep lparen rparen (comma <> space)

encloseSep :: Foldable f => Doc e -> Doc e -> Doc e -> f (Doc e) -> Doc e
encloseSep left right sp ds0 =
  case toList ds0 of
    [] -> left <> right
    [d] -> left <> d <> right
    ds -> left <> align (cat (zipWith (<>) (init ds) (repeat sp) ++ [last ds <> right]))

type Label = String

type Var = Ident
