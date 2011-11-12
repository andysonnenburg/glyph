{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.NestedFuns
       ( module Language.Glyph.Annotation.NestedFuns.Class
       , module Language.Glyph.Annotation.With
       , NestedFuns
       , withNestedFuns
       ) where

import Language.Glyph.Annotation.NestedFuns.Class hiding (NestedFuns)
import qualified Language.Glyph.Annotation.NestedFuns.Class as Class
import Language.Glyph.Annotation.With

newtype NestedFuns
  = NestedFuns { unNestedFuns :: Class.NestedFuns
               } deriving Show

withNestedFuns :: a -> Class.NestedFuns -> With a NestedFuns
withNestedFuns a = With a . NestedFuns

instance HasNestedFuns (With a NestedFuns) where
  nestedFuns (With _ x) = unNestedFuns x

instance HasNestedFuns a => HasNestedFuns (With a b) where
  nestedFuns (With x _) = nestedFuns x
