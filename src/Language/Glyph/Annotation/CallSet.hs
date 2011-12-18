{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.CallSet
       ( module Language.Glyph.Annotation
       , module Language.Glyph.Annotation.CallSet.Class
       , CallSet
       , withCallSet
       ) where

import Language.Glyph.Annotation
import Language.Glyph.Annotation.CallSet.Class hiding (CallSet)
import qualified Language.Glyph.Annotation.CallSet.Class as Class

newtype CallSet
  = CallSet { unCallSet :: Class.CallSet
            } deriving Show

withCallSet :: Class.CallSet -> a -> Annotated CallSet a
withCallSet = Annotated . CallSet

instance HasCallSet (Annotated CallSet a) where
  callSet (Annotated x _) = unCallSet x

instance HasCallSet b => HasCallSet (Annotated a b) where
  callSet (Annotated _ x) = callSet x
