module Language.Glyph.ExtraSet
       ( module Language.Glyph.Annotation.ExtraSet
       , addExtraSet
       ) where

import Control.Applicative
import Control.Comonad

import Data.Data
import Data.Generics
import Data.Maybe
import Data.Monoid

import Language.Glyph.Annotation.ExtraSet
import Language.Glyph.Annotation.Name
import Language.Glyph.Annotation.NameSet (NameSet)
import qualified Language.Glyph.Annotation.NameSet as NameSet
import Language.Glyph.Location
import Language.Glyph.Syntax

addExtraSet :: (HasName a, Monad m) =>
              [Located (Stmt Located a)] ->
              m [Located (Stmt Located (WithExtraSet a))]
addExtraSet = return . map (fmap f <$>)
  where
    f a = WithExtraSet a (m!name a)
    m = undefined

(!) = undefined

addFreeVars :: [Stmt Located a] -> [Stmt Located (WithFreeVars a)]

varsQ :: Data a => a -> NameSet
varsQ = everythingBut (<>) (const (mempty, False) `ext1Q` qStmt `ext1Q` qExpr)
  where
    qStmt :: Stmt Located a -> (NameSet, Bool)
    qStmt (FunDeclS _ _ _) = (mempty, True)
    qStmt _ = (mempty, False)
    
    qExpr :: Data a => Expr Located a -> (NameSet, Bool)
    qExpr (FunE _ _ _) = (mempty, True)
    qExpr (VarE a) = (NameSet.singleton . nameQ $ a, False)
    qExpr _ = (mempty, False)

declsQ :: Data a => a -> NameSet
declsQ = everythingBut (<>) (const (mempty, False) `ext1Q` qStmt)
  where
    qStmt (FunDeclS a _ _) = 

nameQ :: Data a => a -> Name
nameQ = fromJust . something (Nothing `mkQ` q)
  where
    q x@(Name _) = Just x

everythingThisFun :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingThisFun = undefined

type FreeVars = NameSet

class HasFreeVars a where
  freeVars :: a -> FreeVars

data WithFreeVars a = WithFreeVars a FreeVars

instance HasFreeVars (WithFreeVars a) where
  freeVars (WithFreeVars _ x) = x

instance HasCallSet a => HasCallSet (WithFreeVars a) where
  callSet (WithFreeVars x _) = callSet x

type CallSet = NameSet

class HasCallSet a where
  callSet :: a -> CallSet

data WithCallSet a = WithCallSet a CallSet

instance HasCallSet (WithCallSet a) where
  callSet (WithCallSet _ x) = x

instance HasFreeVars a => HasFreeVars (WithCallSet a) where
  freeVars (WithCallSet x _) = freeVars x

(<>) :: Monoid a => a -> a -> a
(<>) = mappend
{-# INLINE (<>) #-}