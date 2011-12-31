{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Language.Glyph.Symtab
       ( module X
       , SymtabT
       , runSymtabT
       , runSymtabT'
       ) where

import Control.Applicative
import Control.Monad.Reader

import Language.Glyph.Ident
import Language.Glyph.IdentMap
import Language.Glyph.Symtab.Class as X
import Language.Glyph.Symtab.Instances ()

newtype SymtabT r m a =
  SymtabT ( ReaderT (IdentMap r) m a
          ) deriving ( Functor
                     , Applicative
                     , Monad
                     , MonadIO
                     , MonadTrans
                     , MonadFix
                     )

runSymtabT :: Monad m => SymtabT r m a -> IdentMap r -> m a
runSymtabT (SymtabT m) = runReaderT m

runSymtabT' :: Monad m => IdentMap r -> SymtabT r m a -> m a
runSymtabT' = flip runSymtabT

instance Monad m => MonadSymtab r (SymtabT r m) where
  askSymtab = SymtabT ask

instance MonadReader r' m => MonadReader r' (SymtabT r m) where
  ask = lift ask
  local f (SymtabT m) = SymtabT $ (mapReaderT . local) f m

instance MonadIdentSupply m => MonadIdentSupply (SymtabT r m) where
  newIdent = lift newIdent
