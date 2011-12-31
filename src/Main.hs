{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Control.Monad.Error hiding (ErrorT (..))

import qualified Data.ByteString.Lazy as ByteString
import Data.Generics

import Language.Glyph.AddCallSet
import Language.Glyph.AddExtraSet
import Language.Glyph.AddFreeVars
import Language.Glyph.AddName
import Language.Glyph.AddSort
import Language.Glyph.CheckBreak
import Language.Glyph.CheckContinue
import Language.Glyph.CheckFun
import Language.Glyph.CheckReturn
import Language.Glyph.CheckVar
import Language.Glyph.Error
import Language.Glyph.Ident
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.InferType
import Language.Glyph.Logger
import Language.Glyph.Monoid
import Language.Glyph.Parse
import Language.Glyph.Rename

import System.Console.CmdArgs hiding (args, name)
import System.Environment
import System.IO

import Prelude hiding (catch, lex)

data Glyph
  = Glyph {} deriving (Typeable, Data)

glyph :: String -> Glyph
glyph progName = modes [Glyph {} &= auto] &= program progName

main :: IO ()
main = do
  progName <- getProgName
  Glyph {} <- cmdArgs (glyph progName)
  glyph'

glyph' :: IO ()
glyph' =
  ByteString.getContents >>=
  runLoggerT . runIdentSupplyT .
  (runErrorT . parse >=>
   addEmptySymtab >=>
   checkBreak >=>
   checkContinue >=>
   checkReturn >=>
   rename >=>
   addSymtab >=>
   addSort >=>
   checkFun >=>
   addFreeVars >=>
   addCallSet >=>
   addExtraSet >=>
   addName >=>
   checkVar >=>
   inferType >=>
   const (return ()))
  where
    addEmptySymtab stmts = return (stmts, ())
    addSymtab (stmts, _) = return (stmts, idents stmts)
    idents = everything (<>) (mempty `mkQ` q)
      where
        q n = IdentMap.singleton n ()
