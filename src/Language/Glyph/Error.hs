{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , UndecidableInstances #-}
module Language.Glyph.Error
       ( ErrorT
       , runErrorT
       ) where

import Compiler.Hoopl

import Control.Applicative
import Control.Exception
import Control.Monad.Error hiding (ErrorT (..))
import qualified Control.Monad.Error as Error

newtype ErrorT e m a
  = ErrorT { unErrorT :: Error.ErrorT e m a
           } deriving ( Functor
                      , Applicative
                      , Monad
                      , MonadIO
                      , MonadTrans
                      )

deriving instance (Error e, Monad m) => MonadError e (ErrorT e m)

runErrorT :: Exception e => MonadIO m => ErrorT e m a -> m a
runErrorT =
  either (liftIO . throwIO) return <=<
  Error.runErrorT .
  unErrorT

instance (Error e, UniqueMonad m) => UniqueMonad (ErrorT e m) where
  freshUnique = lift freshUnique
