{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.FreeVars
       ( module Language.Glyph.Annotation.FreeVars.Class
       , module Language.Glyph.Annotation.With
       , FreeVars
       , withFreeVars
       ) where

import Language.Glyph.Annotation.FreeVars.Class hiding (FreeVars)
import qualified Language.Glyph.Annotation.FreeVars.Class as Class
import Language.Glyph.Annotation.With

newtype FreeVars
  = FreeVars { unFreeVars :: Class.FreeVars
             } deriving Show

withFreeVars :: a -> Class.FreeVars -> With a FreeVars
withFreeVars a = With a . FreeVars

instance HasFreeVars (With a FreeVars) where
  freeVars (With _ x) = unFreeVars x

instance HasFreeVars a => HasFreeVars (With a b) where
  freeVars (With x _) = freeVars x
