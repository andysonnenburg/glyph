module Language.Glyph.Unique.Internal
       ( Unique
       , intToUnique
       , uniqueToInt
       ) where

import qualified Compiler.Hoopl as Hoopl
import Compiler.Hoopl.GHC (uniqueToInt)

import qualified Unsafe.Coerce as Unsafe (unsafeCoerce)

type Unique = Hoopl.Unique

intToUnique :: Int -> Unique
intToUnique = Unsafe.unsafeCoerce

