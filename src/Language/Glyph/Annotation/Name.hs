{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Glyph.Annotation.Name
       ( module Language.Glyph.Annotation
       , module Language.Glyph.Annotation.Name.Class
       , Name
       , withName
       ) where

import Language.Glyph.Annotation
import Language.Glyph.Annotation.Name.Class hiding (Name)
import qualified Language.Glyph.Annotation.Name.Class as Class

newtype Name
  = Name { unName :: Class.Name
         } deriving Show

withName :: Class.Name -> a -> Annotated Name a
withName = Annotated . Name

instance HasName (Annotated Name a) where
  name (Annotated x _) = unName x

instance HasName b => HasName (Annotated a b) where
  name (Annotated _ x) = name x