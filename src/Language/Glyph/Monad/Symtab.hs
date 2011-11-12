{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Glyph.Monad.Symtab
       ( module Language.Glyph.Monad.Symtab.Class
       , SymtabT
       , runSymtabT
       ) where

import Control.Monad.Reader
import Control.Monad.State

import Language.Glyph.Location
import qualified Language.Glyph.Monad as Monad
import Language.Glyph.Monad.Symtab.Class
import Language.Glyph.Name
import Language.Glyph.NameMap (NameMap)
import Language.Glyph.Syntax

newtype SymtabT m i j a
  = SymtabT { unSymtabT :: StateT (S j) (ReaderT R m) a
            } deriving Functor

type S = NameMap
type R = [Located (Stmt Name)]

runSymtabT :: Monad m => SymtabT m i j a -> [Located (Stmt Name)] -> m a
runSymtabT (SymtabT m) ss = runReaderT (evalStateT m initState) ss
  where
    initState = undefined

instance Monad m => Monad.Monad (SymtabT m) where
  (SymtabT m) >>= k = SymtabT $ m >>= unSymtabT . k

instance Monad m => MonadSymtab (SymtabT m)
