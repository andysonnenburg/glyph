{-# LANGUAGE DeriveDataTypeable #-}
module Language.Glyph.Type
       ( TypeScheme (..)
       , Type (..)
       , Label
       , Var
       , showTypes
       ) where

import Control.Monad.State

import Data.Data
import Data.List
import Data.Map (Map)

import Language.Glyph.Ident.Internal
import qualified Language.Glyph.IdentMap as IdentMap

import Prelude hiding (min)

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

showTypes :: (Type, Type) -> (String, String)
showTypes = go
  where
    go (a, b) =
      flip evalState (0, IdentMap.empty) $
      liftM2 (,) (showTypeM a) (showTypeM b)
    showTypeM tau =
      case tau of
        Var x ->
          showVarM x
        a :->: b -> do
          a' <- showTypeM a
          b' <- showTypeM b
          return $ a' ++ " -> " ++ b'
        Int ->
          return "int"
        Double ->
          return "double"
        Bool ->
          return "bool"
        Void ->
          return "void"
        Tuple xs -> do
          xs' <- mapM showTypeM xs
          return $ "(" ++ intercalate ", " xs' ++ ")"
        Cont a -> do
          a' <- showTypeM a
          return $ "Cont# " ++ a'
    showVarM x = do
      (a, m) <- get
      case IdentMap.lookup x m of
        Nothing -> do
          let (q, r) = a `quotRem` size
              s = '\'' : toEnum (r + min) : if q == 0 then [] else show q
          put (a + 1, IdentMap.insert x s m)
          return s
        Just s ->
          return s
    min = fromEnum 'a'
    size = fromEnum 'z' - fromEnum 'a' + 1


type Label = String

type Var = Ident
