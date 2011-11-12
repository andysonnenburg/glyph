{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.VarDecls
       ( module Language.Glyph.Annotation.VarDecls.Class
       , module Language.Glyph.Annotation.With
       , VarDecls
       , withVarDecls
       ) where

import Language.Glyph.Annotation.VarDecls.Class hiding (VarDecls)
import qualified Language.Glyph.Annotation.VarDecls.Class as Class
import Language.Glyph.Annotation.With

newtype VarDecls
  = VarDecls { unVarDecls :: Class.VarDecls
             } deriving Show

withVarDecls :: a -> Class.VarDecls -> With a VarDecls
withVarDecls a = With a . VarDecls

instance HasVarDecls (With a VarDecls) where
  varDecls (With _ x) = unVarDecls x

instance HasVarDecls a => HasVarDecls (With a b) where
  varDecls (With x _) = varDecls x
