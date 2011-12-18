{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.FreeVars
       ( module Language.Glyph.Annotation
       , module Language.Glyph.Annotation.FreeVars.Class
       , FreeVars
       , withFreeVars
       ) where

import Language.Glyph.Annotation
import Language.Glyph.Annotation.FreeVars.Class hiding (FreeVars)
import qualified Language.Glyph.Annotation.FreeVars.Class as Class

newtype FreeVars
  = FreeVars { unFreeVars :: Class.FreeVars
             } deriving Show

withFreeVars :: Class.FreeVars -> a -> Annotated FreeVars a
withFreeVars = Annotated . FreeVars

instance HasFreeVars (Annotated FreeVars a) where
  freeVars (Annotated x _) = unFreeVars x

instance HasFreeVars b => HasFreeVars (Annotated a b) where
  freeVars (Annotated _ x) = freeVars x
