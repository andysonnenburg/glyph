{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , PatternGuards
  , ScopedTypeVariables #-}
module Language.Glyph.CheckFun
       ( CheckFunException (..)
       , checkFun
       ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Writer hiding ((<>))

import Data.Generics
import qualified Data.Text as Text

import Language.Glyph.Record hiding (Sort, name)
import qualified Language.Glyph.Record as Record
import Language.Glyph.Sort
import Language.Glyph.Msg
import qualified Language.Glyph.Msg as Msg
import Language.Glyph.IdentMap (IdentMap, (!))
import Language.Glyph.Pretty
import Language.Glyph.Syntax

checkFun :: forall a fields sym m .
            ( Data a
            , Pretty a
            , Select Stmts [Stmt a] fields
            , Select Symtab (IdentMap (Record sym)) fields
            , Select Record.Sort Sort sym
            , MonadWriter Msgs m
            ) => Record fields -> m ()
checkFun r =
  checkFun' stmts'
  where
    stmts' = r#.stmts
    symtab' = r#.symtab
    
    checkFun' =
      everything (>>) (return () `mkQ` queryExpr)

    queryExpr :: Expr a -> m ()
    queryExpr (Expr a (AssignE name _))
      | Fun <- (symtab' ! ident name)#.sort =
        tell $ Msg.singleton $ mkErrorMsg a $ AssignmentToFunDecl $ view name
    queryExpr _ =
      return ()

data CheckFunException
  = AssignmentToFunDecl NameView
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show CheckFunException where
  show = showDefault

instance Pretty CheckFunException where
  pretty = go
    where
      go (AssignmentToFunDecl a) =
        text "illegal" </>
        text "assignment" </>
        text "to" </>
        text "fn" </>
        char '`' <> text (Text.unpack a) <> char '\''
      go (StrMsgError s) =
        text s
      go NoMsgError =
        text "internal" </>
        text "error"

instance Exception CheckFunException

instance Error CheckFunException where
  strMsg = StrMsgError
  noMsg = NoMsgError
