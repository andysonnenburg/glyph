{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.ExtraSet
       ( module Language.Glyph.Annotation
       , module Language.Glyph.Annotation.ExtraSet.Class
       , ExtraSet
       , withExtraSet
       ) where

import Language.Glyph.Annotation
import Language.Glyph.Annotation.ExtraSet.Class hiding (ExtraSet)
import qualified Language.Glyph.Annotation.ExtraSet.Class as Class

newtype ExtraSet
  = ExtraSet { unExtraSet :: Class.ExtraSet
             } deriving Show

withExtraSet :: Class.ExtraSet -> a -> Annotated ExtraSet a
withExtraSet = Annotated . ExtraSet

instance HasExtraSet (Annotated ExtraSet a) where
  extraSet (Annotated x _) = unExtraSet x

instance HasExtraSet b => HasExtraSet (Annotated a b) where
  extraSet (Annotated _ x) = extraSet x
