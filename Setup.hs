module Main (main) where

import Control.Applicative

import Distribution.Simple (defaultMainArgs)

import System.Environment (getArgs)

main :: IO ()
main =
  updateArgs <$> getArgs >>=
  defaultMainArgs
  where
    updateArgs ("configure" : args) =
      "configure" : "--alex-options=--template=templates" : args
    updateArgs args =
      args

