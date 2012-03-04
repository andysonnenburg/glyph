{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.Type
       ( module X
       ) where

import Language.Glyph.Annotation as X
import Language.Glyph.Annotation.Type.Class as X
import Language.Glyph.Syntax.Internal
import Language.Glyph.Type as X

instance HasType Type where
  type' = id

instance HasType (Annotated Type a) where
  type' (Annotated x _) = x

instance HasType b => HasType (Annotated a b) where
  type' (Annotated _ x) = type' x

instance HasType a => HasType (Stmt a) where
  type' (Stmt x _) = type' x

instance HasType a => HasType (Expr a) where
  type' (Expr x _) = type' x
