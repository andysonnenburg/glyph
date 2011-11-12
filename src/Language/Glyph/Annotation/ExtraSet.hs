{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.ExtraSet
       ( module Language.Glyph.Annotation.ExtraSet.Class
       , module Language.Glyph.Annotation.With
       , ExtraSet
       , withExtraSet
       ) where

import Language.Glyph.Annotation.ExtraSet.Class hiding (ExtraSet)
import qualified Language.Glyph.Annotation.ExtraSet.Class as Class
import Language.Glyph.Annotation.With

newtype ExtraSet
  = ExtraSet { unExtraSet :: Class.ExtraSet
             } deriving Show

withExtraSet :: a -> Class.ExtraSet -> With a ExtraSet
withExtraSet a = With a . ExtraSet

instance HasExtraSet (With a ExtraSet) where
  extraSet (With _ x) = unExtraSet x

instance HasExtraSet a => HasExtraSet (With a b) where
  extraSet (With x _) = extraSet x