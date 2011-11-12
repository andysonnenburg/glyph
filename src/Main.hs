{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ViewPatterns #-}
module Main (main) where

import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.ByteString.Lazy as ByteString
import Data.Generics

import Language.Glyph.AddCallSet
import Language.Glyph.AddExtraSet
import Language.Glyph.AddFreeVars
import Language.Glyph.AddSort
import Language.Glyph.CheckFun
import Language.Glyph.CheckVar
import Language.Glyph.ErrorIO
import Language.Glyph.Logger
import Language.Glyph.Monoid
import Language.Glyph.Identify
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Parse

import System.Console.CmdArgs hiding (name)
import System.Environment
import System.IO

import Prelude hiding (catch, lex)

data Glyph = Glyph {} deriving (Typeable, Data)

glyph :: String -> Glyph
glyph progIdent = Glyph {} &= program progIdent

main :: IO ()
main = do
  progName <- getProgName
  Glyph {..} <- cmdArgs (glyph progName)
  stmts <- ByteString.getContents >>= parse'
  runLoggerT $ do
    stmts' <- identify stmts
    let runReaderT' m = runReaderT m stmts'
    _ <- runReaderT' $ do
      ($ identsQ stmts') $
        addSort >=>
        checkFun >=>
        addFreeVars >=>
        addCallSet >=>
        addExtraSet >=>
        checkVar
    liftIO $ print stmts'
  where
    parse' = runErrorIO . parse

identsQ :: Data a => a -> IdentMap ()
identsQ = everything (<>) (mempty `mkQ` q)
  where
    q n = IdentMap.singleton n ()
