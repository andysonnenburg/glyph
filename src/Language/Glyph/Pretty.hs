module Language.Glyph.Pretty
       ( module X
       , showDefault
       , tupled
       , encloseSep
       ) where

import Data.Foldable

import Text.PrettyPrint.Free as X hiding (tupled, encloseSep)

showDefault :: Pretty a => a -> String
showDefault = show . pretty

tupled :: Foldable f => f (Doc e) -> Doc e
tupled = encloseSep lparen rparen (comma <> space)

encloseSep :: Foldable f => Doc e -> Doc e -> Doc e -> f (Doc e) -> Doc e
encloseSep left right sp ds0 =
  case toList ds0 of
    [] -> left <> right
    [d] -> left <> d <> right
    ds -> left <> align (hcat (zipWith (<>) (init ds) (repeat sp) ++ [last ds <> right]))
