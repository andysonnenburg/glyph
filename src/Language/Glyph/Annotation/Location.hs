{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.Location
       ( module Language.Glyph.Annotation
       , module Language.Glyph.Annotation.Location.Class
       ) where

import Language.Glyph.Annotation
import Language.Glyph.Annotation.Location.Class
import Language.Glyph.HM.Syntax
import Language.Glyph.Syntax.Internal

instance HasLocation Location where
  location = id

instance HasLocation (Annotated Location a) where
  location (Annotated x _) = x

instance HasLocation b => HasLocation (Annotated a b) where
  location (Annotated _ x) = location x

instance HasLocation a => HasLocation (Stmt a) where
  location (Stmt x _) = location x

instance HasLocation a => HasLocation (Expr a) where
  location (Expr x _) = location x

instance HasLocation a => HasLocation (Exp a) where
  location (Exp x _) = location x
