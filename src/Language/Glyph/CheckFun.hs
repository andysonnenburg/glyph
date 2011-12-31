{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , PatternGuards
  , ScopedTypeVariables
  , ViewPatterns #-}
module Language.Glyph.CheckFun
       ( CheckFunException (..)
       , checkFun
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader

import Data.Generics
import qualified Data.Text as Text

import Language.Glyph.Annotation.Sort
import Language.Glyph.Location
import Language.Glyph.Logger
import Language.Glyph.Message
import Language.Glyph.IdentMap (IdentMap, (!))
import Language.Glyph.Syntax

checkFun :: forall a b m .
           ( Data a
           , HasLocation a
           , HasSort b
           , MonadLogger Message m
           ) => ([Stmt a], IdentMap b) -> m ([Stmt a], IdentMap b)
checkFun (stmts, symtab) = do
  checkFun' stmts
  return (stmts, symtab)
  where
    checkFun' = everything (>>) (return () `mkQ` queryExpr)

    queryExpr :: Expr a -> m ()
    queryExpr x@(view -> AssignE name _)
      | Fun <- sort (symtab !ident name) =
        runReaderT' $ logError $ AssignmentToFunDecl (view name)
      where
        runReaderT' m = runReaderT m (location x)
    queryExpr _ =
      return ()

data CheckFunException
  = AssignmentToFunDecl NameView
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show CheckFunException where
  show x =
    case x of
      AssignmentToFunDecl a ->
        "illegal assignment to fun `" ++ Text.unpack a ++ "'"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception CheckFunException

instance Error CheckFunException where
  strMsg = StrMsgError
  noMsg = NoMsgError
