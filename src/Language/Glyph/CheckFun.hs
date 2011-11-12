{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , PatternGuards #-}
module Language.Glyph.CheckFun
       ( CheckFunException (..)
       , checkFun
       ) where

import Control.Applicative
import Control.Comonad
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Generics
import qualified Data.Text as Text

import Language.Glyph.Annotation.Sort
import Language.Glyph.Location
import Language.Glyph.Message
import Language.Glyph.Ident
import Language.Glyph.IdentMap (IdentMap, (!))
import Language.Glyph.Syntax

checkFun :: ( HasSort a
            , MonadReader [Located (Stmt Ident)] m
            , MonadWriter Message m
            ) => IdentMap a -> m (IdentMap a)
checkFun symtab = do
  ask >>= checkFunQ symtab
  return symtab

data CheckFunException
  = AssignmentToFunDecl (Located Name)
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show CheckFunException where
  show x =
    case x of
      AssignmentToFunDecl a ->
        show (location a) ++ ": illegal assignment to fun `" ++
        Text.unpack (extract a) ++ "'"
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception CheckFunException

instance Error CheckFunException where
  strMsg = StrMsgError
  noMsg = NoMsgError

checkFunQ :: (HasSort a, Data b, MonadWriter Message m) => IdentMap a -> b -> m ()
checkFunQ symtab = everything (>>) (return () `mkQ` q)
  where
    q (AssignE x _) | Fun <- sort (symtab!ident x) =
      tell $ Error $ AssignmentToFunDecl (name <$> x)
    q _ =
      return ()
    
    ident = extract . extract