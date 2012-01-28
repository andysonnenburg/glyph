{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , UndecidableInstances #-}
module Language.Glyph.UniqueSupply
       ( module X
       , UniqueSupply
       , runUniqueSupply
       , UniqueSupplyT
       , runUniqueSupplyT
       ) where

import qualified Compiler.Hoopl as Hoopl
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State

import Language.Glyph.Logger.Class
import Language.Glyph.Unique.Internal
import Language.Glyph.UniqueSupply.Class as X

import qualified Unsafe.Coerce as Unsafe (unsafeCoerce)

import Prelude hiding (log)

type UniqueSupply = UniqueSupplyT Identity

runUniqueSupply :: UniqueSupply a -> a
runUniqueSupply = runIdentity . runUniqueSupplyT

newtype UniqueSupplyT m a
  = UniqueSupplyT { unUniqueSupplyT :: StateT Int m a
                  } deriving ( Functor
                             , Applicative
                             , Monad
                             , MonadTrans
                             , MonadIO
                             )

deriving instance MonadError e m => MonadError e (UniqueSupplyT m)

runUniqueSupplyT :: Monad m => UniqueSupplyT m a -> m a
runUniqueSupplyT = flip evalStateT 0 . unUniqueSupplyT

instance Monad m => MonadUniqueSupply (UniqueSupplyT m) where
  freshUnique = do
    x <- get
    put $ x + 1
    return $ intToUnique x

get :: Monad m => UniqueSupplyT m Int
get = UniqueSupplyT State.get

put :: Monad m => Int -> UniqueSupplyT m ()
put = UniqueSupplyT . State.put

instance MonadUniqueSupply m => MonadUniqueSupply (ReaderT r m) where
  freshUnique = lift freshUnique

instance MonadLogger w m => MonadLogger w (UniqueSupplyT m) where
  log = lift . log

instance Monad m => Hoopl.UniqueMonad (UniqueSupplyT m) where
  freshUnique = liftM Unsafe.unsafeCoerce freshUnique