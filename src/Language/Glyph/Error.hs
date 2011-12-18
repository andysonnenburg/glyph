{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Language.Glyph.Error
       ( ErrorT (..)
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error hiding (ErrorT (..))

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
  catchError (ErrorT m) h = undefined -- ErrorT $ catch m (runErrorIO . h)

instance MonadTrans (ErrorT s) where
  lift = ErrorT