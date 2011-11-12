{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.Sort
       ( module Language.Glyph.Annotation.Sort.Class
       , module Language.Glyph.Annotation.With
       ) where

import Language.Glyph.Annotation.Sort.Class
import Language.Glyph.Annotation.With

instance HasSort (With a Sort) where
  sort (With _ x) = x

instance HasSort a => HasSort (With a b) where
  sort (With x _) = sort x
