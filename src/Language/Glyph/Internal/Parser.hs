{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RecordWildCards
  , StandaloneDeriving
  , UndecidableInstances #-}
module Language.Glyph.Internal.Parser
       ( ParserT
       , ParseException (..)
       , runParserT
       , getPosition
       , putPosition
       , getBuffer
       , putBuffer
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State

import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding.Error
import Data.Typeable

import Language.Glyph.Internal.Location

newtype ParserT m a
  = ParserT { unParserT :: StateT S m a
            } deriving ( Functor
                       , Applicative
                       )

deriving instance MonadError ParseException m => MonadError ParseException (ParserT m)

instance MonadError ParseException m => Monad (ParserT m) where
  return = ParserT . return
  (ParserT m) >>= k = ParserT $ m >>= unParserT . k
  fail msg = do
    p <- getPosition
    let l = Location p p
    throwError $ ParseError l msg

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

getPosition :: Monad m => ParserT m Position
getPosition = gets position

putPosition :: Monad m => Position -> ParserT m ()
putPosition position = modify $ \ s -> s { position }

getBuffer :: Monad m => ParserT m ByteString
getBuffer = gets buffer

putBuffer :: Monad m => ByteString -> ParserT m ()
putBuffer buffer = modify $ \ s -> s { buffer }

gets :: Monad m => (S -> a) -> ParserT m a
gets = ParserT . State.gets

modify :: Monad m => (S -> S) -> ParserT m ()
modify = ParserT . State.modify