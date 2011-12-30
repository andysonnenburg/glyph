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
    go (a, b) = flip evalState ('a', IdentMap.empty) $ liftM2 (,) (f a) (f b)
    f (Var x) = showVarM x
    f (a :->: b) = do
      a' <- f a
      b' <- f b
      return $ a' ++ " -> " ++ b'
    f Int = return "Int"
    f Double = return "Double"
    f Bool = return "Bool"
    f Void = return "Void"
    f (Tuple xs) = do
      xs' <- mapM f xs
      return $ "(" ++ intercalate ", " xs' ++ ")"
    f (Cont a) = liftM ("Cont#" ++) $ f a
    showVarM x = do
      (a, m) <- get
      case IdentMap.lookup x m of
        Nothing -> do
          let v = ['#', a]
          put (succ a, IdentMap.insert x v m)
          return v
        Just s ->
          return s


type Label = String

type Var = Ident