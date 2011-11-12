{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.CallSet
       ( module Language.Glyph.Annotation.CallSet.Class
       , module Language.Glyph.Annotation.With
       , CallSet
       , withCallSet
       ) where

import Language.Glyph.Annotation.CallSet.Class hiding (CallSet)
import qualified Language.Glyph.Annotation.CallSet.Class as Class
import Language.Glyph.Annotation.With

newtype CallSet
  = CallSet { unCallSet :: Class.CallSet
            } deriving Show

withCallSet :: a -> Class.CallSet -> With a CallSet
withCallSet a = With a . CallSet

instance HasCallSet (With a CallSet) where
  callSet (With _ x) = unCallSet x

instance HasCallSet a => HasCallSet (With a b) where
  callSet (With x _) = callSet x
