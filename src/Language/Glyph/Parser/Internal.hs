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
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State

import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding.Error
import Data.Typeable

import Language.Glyph.Annotation.Location
import Language.Glyph.Unique

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
    let l = Location position position
    throwError $ ParseError l msg

instance ( MonadError ParseException m
         , UniqueMonad m
         ) => UniqueMonad (ParserT m) where
  freshUnique = lift freshUnique

data ParseException
  = ParseError Location String
  | LexicalError Location
  | UnicodeError Location UnicodeException
  | StrMsgError String
  | NoMsgError deriving Typeable

instance Show ParseException where
  show x =
    case x of
      ParseError l s -> show l ++ ": parse error: " ++ s
      LexicalError l -> show l ++ ": lexical error"
      UnicodeError l e -> show l ++ ": unicode error: " ++ show e
      StrMsgError s -> s
      NoMsgError -> "internal error"

instance Exception ParseException

instance Error ParseException where
  strMsg = StrMsgError
  noMsg = NoMsgError

data S
  = S { position :: Position
      , buffer :: ByteString
      }

runParserT :: Monad m => ParserT m a -> ByteString -> m a
runParserT (ParserT m) buffer = evalStateT m initState
  where
    initState =
      S { position = Position 1 1
        , buffer
        }

get :: Monad m => ParserT m S
get = ParserT State.get

put :: Monad m => S -> ParserT m ()
put = ParserT . State.put
