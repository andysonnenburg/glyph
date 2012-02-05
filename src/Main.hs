{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}
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
import Language.Glyph.Hoopl (toGraph, showGraph')
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.InferType
import Language.Glyph.Logger
import Language.Glyph.Monoid
import Language.Glyph.Parse
import Language.Glyph.Rename
import Language.Glyph.Syntax
import Language.Glyph.Unique

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
  runLoggerT . runUniqueSupplyT .
  (runErrorT . parse >=>
   mkSymtab >=>
   checkBreak >=>
   checkContinue >=>
   checkReturn >=>
   rename >=>
   addIdents >=>
   addSort >=>
   checkFun >=>
   addFreeVars >=>
   addCallSet >=>
   addExtraSet >=>
   addName >=>
   checkVar >=>
   (\ (stmts, _symtab) -> do
     graph <- toGraph' stmts
     liftIO $ putStrLn $ showGraph' graph
     return ()))
  where
    mkSymtab stmts = return (stmts, ())
    addIdents (stmts, _) = return (stmts, idents stmts)
    idents :: forall a . Data a => [Stmt a] -> IdentMap ()
    idents = everything (<>) (mempty `mkQ` queryExpr `extQ` queryName)
      where
        queryExpr :: ExprView a -> IdentMap ()
        queryExpr (FunE x _ _) = IdentMap.singleton x ()
        queryExpr _ = mempty
        queryName (ident -> x) = IdentMap.singleton x ()
    toGraph' = runErrorT . toGraph
