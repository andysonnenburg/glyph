{-# LANGUAGE
    DeriveDataTypeable
  , RecordWildCards
  , ScopedTypeVariables
  , ViewPatterns #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import Control.Monad.Error hiding (ErrorT (..))

import qualified Data.ByteString.Lazy as ByteString
import Data.Generics
import Data.Monoid

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
import Language.Glyph.HM (inferType)
import Language.Glyph.IR (fromStmts, showGraph', toHM)
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Logger
import Language.Glyph.Parse
import Language.Glyph.Rename
import Language.Glyph.Syntax
import Language.Glyph.Unique

import System.Console.CmdArgs hiding (args)
import System.Environment
import System.IO

import Prelude hiding (catch, lex)

data Glyph
  = Glyph { dumpIR :: Bool
          , dumpHM :: Bool
          } deriving (Typeable, Data)

glyphCmdArgs :: String -> Glyph
glyphCmdArgs progName = modes [Glyph { dumpIR = def &= explicit &= name "dump-ir"
                                     , dumpHM = def &= explicit &= name "dump-hm"
                                     } &= auto] &= program progName

main :: IO ()
main = do
  progName <- getProgName
  args <- cmdArgs (glyphCmdArgs progName)
  glyph args

glyph :: Glyph -> IO ()
glyph Glyph {..} =
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
   (\ (stmts, symtab) -> do
     ir <- fromStmts' stmts
     when dumpIR $
       liftIO $ putStrLn $ showGraph' ir
     hm <- toHM ir symtab
     when dumpHM $
       liftIO $ print hm
     -- _ <- inferType hm
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
    fromStmts' = runErrorT . fromStmts
