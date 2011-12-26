{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , TemplateHaskell
  , UndecidableInstances #-}
module Language.Glyph.Ident.Internal
       ( Ident (..)
       , MonadIdentSupply (..)
       , IdentSupply
       , runIdentSupply
       , IdentSupplyT
       , runIdentSupplyT
       ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Data

import Language.Glyph.Error
import Language.Glyph.Logger
import Language.Haskell.TH.Syntax (showName)
import qualified Language.Haskell.TH as TH

newtype Ident
  = Ident { unIdent :: Int
          } deriving (Show, Eq, Ord, Typeable)

instance Data Ident where
  gfoldl _f z = z
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType name
    where
      name = $(return . TH.LitE . TH.StringL . showName $ ''Ident)

class Monad m => MonadIdentSupply m where
  newIdent :: m Ident

type IdentSupply = IdentSupplyT Identity

runIdentSupply :: IdentSupply a -> a
runIdentSupply = runIdentity . runIdentSupplyT

newtype IdentSupplyT m a =
  IdentSupplyT { unIdentSupplyT :: StateT Int m a
               } deriving ( Functor
                          , Applicative
                          , Monad
                          , MonadTrans
                          , MonadIO
                          )
deriving instance MonadLogger w m => MonadLogger w (IdentSupplyT m)
deriving instance MonadReader r m => MonadReader r (IdentSupplyT m)
deriving instance MonadWriter w m => MonadWriter w (IdentSupplyT m)

runIdentSupplyT :: Monad m => IdentSupplyT m a -> m a
runIdentSupplyT = flip evalStateT 0 . unIdentSupplyT

instance Monad m => MonadIdentSupply (IdentSupplyT m) where
  newIdent = IdentSupplyT $ do
    s <- get
    put $ s + 1
    return $ Ident s

instance MonadIdentSupply m => MonadIdentSupply (ErrorT e m) where
  newIdent = lift newIdent

instance MonadIdentSupply m => MonadIdentSupply (ReaderT r m) where
  newIdent = lift newIdent

instance MonadIdentSupply m => MonadIdentSupply (StateT s m) where
  newIdent = lift newIdent
