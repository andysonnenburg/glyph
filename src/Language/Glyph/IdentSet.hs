{-# LANGUAGE NoImplicitPrelude #-}
module Language.Glyph.IdentSet
       ( IdentSet
       , (\\)
       , empty
       , fromList
       , insert
       , intersection
       , isSubsetOf
       , map
       , member
       , notMember
       , null
       , singleton
       , toList
       , union
       , unions
       ) where

import Language.Glyph.IdentSet.Internal
