{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.Sort
       ( module Language.Glyph.Annotation
       , module Language.Glyph.Annotation.Sort.Class
       ) where

import Language.Glyph.Annotation
import Language.Glyph.Annotation.Sort.Class

instance HasSort (Annotated Sort a) where
  sort (Annotated x _) = x

instance HasSort b => HasSort (Annotated a b) where
  sort (Annotated _ x) = sort x
