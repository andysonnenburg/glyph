module Language.Glyph.Unique.Internal
       ( module X
       , intToUnique
       , uniqueToInt
       ) where

import Compiler.Hoopl as X (Unique, UniqueMonad (..))
import Compiler.Hoopl.GHC (uniqueToInt)

import qualified Unsafe.Coerce as Unsafe (unsafeCoerce)

intToUnique :: Int -> Unique
intToUnique = Unsafe.unsafeCoerce
