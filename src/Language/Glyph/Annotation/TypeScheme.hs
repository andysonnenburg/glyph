{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.TypeScheme
       ( module X
       ) where

import Language.Glyph.Annotation as X
import Language.Glyph.Annotation.TypeScheme.Class as X
import Language.Glyph.Type as X

instance HasTypeScheme (Annotated TypeScheme a) where
  typeScheme (Annotated x _) = x

instance HasTypeScheme b => HasTypeScheme (Annotated a b) where
  typeScheme (Annotated _ x) = typeScheme x