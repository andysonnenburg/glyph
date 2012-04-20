module Language.Glyph.IdentSet
       ( module X
       , IdentSet
       ) where

import Language.Glyph.Ident
import Language.Glyph.Set
import Language.Glyph.Set as X hiding (Set)

type IdentSet = Set Ident
