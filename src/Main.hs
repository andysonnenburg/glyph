{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , RecordWildCards
  , ScopedTypeVariables
  , TypeOperators
  , ViewPatterns #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import Control.Monad.Error hiding (ErrorT (..))

import qualified Data.ByteString.Lazy as ByteString
import Data.Generics
import qualified Data.HashMap.Strict as Map
import qualified Data.Record as Record

import Language.Glyph.AddCallSet
import Language.Glyph.AddExtraSet
import Language.Glyph.AddFreeVars
import Language.Glyph.AddName
import Language.Glyph.AddSort
import Language.Glyph.CheckBreak
import Language.Glyph.CheckContinue
import Language.Glyph.CheckFun
import Language.Glyph.CheckReturn
import Language.Glyph.HM (inferType)
import qualified Language.Glyph.IR as IR
import Language.Glyph.IR.RemoveUnreachable
import Language.Glyph.IdentMap (IdentMap)
import qualified Language.Glyph.IdentMap as IdentMap
import Language.Glyph.Loc
import Language.Glyph.Logger
import Language.Glyph.Monoid
import Language.Glyph.Parse
import Language.Glyph.Record hiding (Loc)
import Language.Glyph.Rename
import Language.Glyph.Syntax
import Language.Glyph.Type
import Language.Glyph.Unique

import System.Console.CmdArgs hiding ((:=), args, name)
import qualified System.Console.CmdArgs as CmdArgs
import System.Environment
import System.IO

import Text.PrettyPrint.Free

import Prelude hiding (catch, lex)

data Glyph
  = Glyph { dumpIR :: Bool
          , dumpHM :: Bool
          , dumpTypes :: Bool
          } deriving (Typeable, Data)

glyphCmdArgs :: String -> Glyph
glyphCmdArgs progName =
  modes [Glyph { dumpIR = def &= explicit &= CmdArgs.name "dump-ir"
               , dumpHM = def &= explicit &= CmdArgs.name "dump-hm"
               , dumpTypes = def &= explicit &= CmdArgs.name "dump-types"
               } &= auto] &= program progName

main :: IO ()
main = do
  progName <- getProgName
  args <- cmdArgs (glyphCmdArgs progName)
  glyph args

glyph :: Glyph -> IO ()
glyph Glyph {..} =
  ByteString.getContents >>=
  runUniqueSupplyT . runLoggerT .
  
  -- Parse
  (runErrorT . parse >=>
   
   -- Create record with stmts
   (\ xs ->
     return $ stmts #= xs #| Record.empty) >=>
   
   -- Check for illegal break statements
   (\ r -> do
     checkBreak (r#.stmts)
     return r) >=>
   
   -- Check for illegal continue statements
   (\ r -> do
     checkContinue (r#.stmts)
     return r) >=>
   
   -- Check for illegal return statements
   (\ r -> do
     checkReturn (r#.stmts)
     return r) >=>
   
   -- Rename identifiers
   (\ r -> do
     xs <- rename (r#.stmts)
     return $ stmts #= xs #| (r #- stmts)) >=>
   
   -- Add symbol table
   (\ r ->
     return $ symtab #= idents (r#.stmts) #| r) >=>
   
   -- Add identifier sort
   addSort >=>
   
   -- Check for assignment to function declarations
   (\ r -> do
     checkFun r
     return r) >=>
   
   -- Add free variables to symbol table
   addFreeVars >=>
   
   -- Add call set to symbol table
   addCallSet >=>
   
   -- Add extra set to symbol table
   addExtraSet >=>
   
   -- Add variable names to symbol table
   addName >=>
   
   -- Check for variable use before initialization
   -- (\ r -> do
   --   checkVars r 
   --   return r) >=>
   
   -- Convert to intermeidate representation
   (\ r -> do
       ir <- runErrorT $ IR.fromStmts (r#.stmts)
       when dumpIR $
         liftIO $ hPrint stderr ir
       return $ insns #= ir #| r #- stmts) >=>
   
   (\ r -> do
     let r' = insns #= IR.mapGraph'' removeUnreachable (r#.insns) #| r #- insns
     return r') >=>
   
   -- Type check syntax via inference
   (\ r -> do
       let r' = insns #= fmap WrapSemigroup (r#.insns) #| r #- insns
       hm <- liftM (fmap (unwrapSemigroup memptyLoc)) $ IR.toHM r'
       when dumpHM $
         liftIO $ hPrint stderr hm
       gamma <- runErrorT $ inferType hm
       when dumpTypes $ do
         let m = r#.symtab
         runPrettyTypeT $ forM_ (Map.toList gamma) $ \ (x, tau) ->
           whenJust (select name =<< Map.lookup x m) $ \ x' -> do
             tau' <- prettyM tau
             liftIO $ hPrint stderr $ prettyText x' <> colon <+> tau'
       return r') >=>
   
   -- Convert to bytecode representation
   
   -- Assemble
   
   (\ _ ->
     return ()))

idents :: forall a . Data a => [Stmt a] -> IdentMap (Record '[])
idents = everything (<>) (mempty `mkQ` queryExpr `extQ` queryName)
  where
    queryExpr :: ExprView a -> IdentMap (Record '[])
    queryExpr (FunE x _ _) = IdentMap.singleton x Record.empty
    queryExpr _ = mempty
    queryName (ident -> x) = IdentMap.singleton x Record.empty

memptyLoc :: Loc
memptyLoc = Loc initPos initPos

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = go x
  where
    go Nothing = return ()
    go (Just a) = f a