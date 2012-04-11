{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RecordWildCards
  , StandaloneDeriving
  , UndecidableInstances #-}
module Language.Glyph.Parser.Internal
       ( ParserT
       , S (..)
       , ParseException (..)
       , runParserT
       , get
       , put
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.State.Strict (StateT, evalStateT)
import qualified Control.Monad.State.Strict as State

import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding.Error
import Data.Typeable

import Language.Glyph.Loc
import Language.Glyph.Unique

import Text.PrettyPrint.Free

newtype ParserT m a
  = ParserT { unParserT :: StateT S m a
            } deriving ( Functor
                       , Applicative
                       , MonadTrans
                       )

deriving instance MonadError ParseException m => MonadError ParseException (ParserT m)

instance MonadError ParseException m => Monad (ParserT m) where
  return = ParserT . return
  (ParserT m) >>= k = ParserT $ m >>= unParserT . k
  fail msg = do
    S {..} <- get
    let l = Loc pos pos
    throwError $ ParseError l msg

instance ( MonadError ParseException m
         , UniqueMonad m
         ) => UniqueMonad (ParserT m) where
  freshUnique = lift freshUnique

data ParseException
  = ParseError Loc String
  | LexicalError Loc
  | UnicodeError Loc UnicodeException
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show ParseException where
  show = show . pretty

instance Pretty ParseException where
  pretty = go
    where
      go (ParseError l s) =
        pretty l <> char ':' <+>
        text "parse" <+> text "error" <> char ':' <+>
        text s
      go (LexicalError l) = 
        pretty l <> char ':' <+>
        text "lexical" <+> text "error"
      go (UnicodeError l e) = 
        pretty l <> char ':' <+> 
        text "unicode" <+> text "error" <> char ':' <+>
        text (show e)
      go (StrMsgError s) =
        text s
      go NoMsgError =
        text "internal" <+> text "error"

instance Exception ParseException

instance Error ParseException where
  strMsg = StrMsgError
  noMsg = NoMsgError

data S
  = S { pos :: Pos
      , buffer :: ByteString
      }

runParserT :: Monad m => ParserT m a -> ByteString -> m a
runParserT (ParserT m) buffer = evalStateT m initState
  where
    initState =
      S { pos = initPos
        , buffer
        }

get :: Monad m => ParserT m S
get = ParserT State.get

put :: Monad m => S -> ParserT m ()
put = ParserT . State.put
