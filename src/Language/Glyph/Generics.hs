{-# LANGUAGE Rank2Types #-}
module Language.Glyph.Generics
       ( module Data.Generics
       , everythingButFuns
       , everythingThisScope
       ) where

import Data.Generics

import Language.Glyph.Syntax

-- | Summarise all nodes in bottom-up, left-to-right order
-- everything' k f x = k x (foldl k

everythingButFuns :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingButFuns k f =
  everythingBut k (f' `ext1Q` gS `ext1Q` gE)
  where
    f' x = (f x, False)
    
    gS x@(FunDeclS _ _ _) = (f x, True)
    gS x = (f x, False)
    
    gE x@(FunE _ _ _) = (f x, True)
    gE x = (f x, False)

everythingThisScope :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingThisScope k f =
  everythingBut k (f' `ext1Q` gS `ext1Q` gE)
  where
    f' x = (f x, False)
    
    gS x@(FunDeclS _ _ _) = (f x, True)
    gS x@(BlockS _) = (f x, True)
    gS x = (f x, False)
    
    gE x@(FunE _ _ _) = (f x, True)
    gE x = (f x, False)
