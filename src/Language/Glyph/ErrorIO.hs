{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Language.Glyph.ErrorIO
       ( ErrorIO (..)
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error

import Prelude hiding (catch)

newtype ErrorIO s a
  = ErrorIO { runErrorIO :: IO a
            } deriving ( Functor
                       , Applicative
                       , Monad
                       , MonadFix
                       )

instance Exception e => MonadError e (ErrorIO s) where
  throwError = ErrorIO . throwIO
  catchError (ErrorIO m) h = ErrorIO $ catch m (runErrorIO . h)