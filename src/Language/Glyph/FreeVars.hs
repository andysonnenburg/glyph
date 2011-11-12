module Language.Glyph.FreeVars
       ( FreeVars
       , addFreeVars
       ) where

addFreeVars :: (HasSort a, Monad m) =>
              [Located (Stmt a)] ->
              m [Located (Stmt (With FreeVars a))]

