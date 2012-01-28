{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Language.Glyph.Error
       ( ErrorT (..)
       ) where

import qualified Compiler.Hoopl as Hoopl
import Control.Applicative
import Control.Exception
import Control.Monad.Error hiding (ErrorT (..))

import Language.Glyph.UniqueSupply

import Prelude hiding (catch)

newtype ErrorT s m a
  = ErrorT { runErrorT :: m a
           } deriving ( Functor
                       , Applicative
                       , Monad
                       , MonadFix
                       )

instance (Exception e, MonadIO m) => MonadError e (ErrorT s m) where
  throwError = ErrorT . liftIO . throwIO

instance MonadTrans (ErrorT s) where
  lift = ErrorT

instance MonadUniqueSupply m => MonadUniqueSupply (ErrorT s m) where
  freshUnique = lift freshUnique

instance Hoopl.UniqueMonad m => Hoopl.UniqueMonad (ErrorT s m) where
  freshUnique = lift Hoopl.freshUnique