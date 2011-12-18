{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.TypeScheme
       ( module Language.Glyph.Annotation
       , module Language.Glyph.Annotation.TypeScheme.Class
       , module Language.Glyph.Type
       ) where

import Language.Glyph.Annotation
import Language.Glyph.Annotation.TypeScheme.Class
import Language.Glyph.Type

instance HasTypeScheme (Annotated TypeScheme a) where
  typeScheme (Annotated x _) = x

instance HasTypeScheme b => HasTypeScheme (Annotated a b) where
  typeScheme (Annotated _ x) = typeScheme x