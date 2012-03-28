{-# LANGUAGE ExistentialQuantification #-}
module Language.Glyph.Msg
       ( Msg
       , mkWarningMsg
       , mkErrorMsg
       , Msgs
       , singleton
       ) where

import System.Console.Terminfo.PrettyPrint

import Text.PrettyPrint.Free

data Msg = Msg MsgType SomePretty SomePretty

data MsgType
  = Error
  | Warning

instance Pretty MsgType where
  pretty = go
    where
      go Error = text "error"
      go Warning = text "warning"

instance PrettyTerm MsgType where
  prettyTerm x = go x $ pretty x
    where
      go Error = red
      go Warning = yellow

mkWarningMsg :: (Pretty a, Pretty b) => a -> b -> Msg
mkWarningMsg x y = Msg Warning (SomePretty x) (SomePretty y)

mkErrorMsg :: (Pretty a, Pretty b) => a -> b -> Msg
mkErrorMsg x y = Msg Error (SomePretty x) (SomePretty y)

type Msgs = [Msg]

singleton :: Msg -> Msgs
singleton x = [x]

instance Show Msg where
  show = show . pretty

instance Pretty Msg where
  pretty (Msg typ x y) =
    pretty typ <> char ':' <+> pretty x <> char ':' <+> pretty y

data SomePretty = forall a . Pretty a => SomePretty a

instance Pretty SomePretty where
  pretty (SomePretty x) = pretty x
