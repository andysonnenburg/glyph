module Language.Glyph.Stream
       ( Stream (..)
       , enumFrom
       ) where

import Prelude hiding (enumFrom)

data Stream a = a :| Stream a

enumFrom :: Enum a => a -> Stream a
enumFrom a = a :| enumFrom (succ a)
