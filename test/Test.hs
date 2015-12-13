{-# LANGUAGE QuasiQuotes #-}

module Main
       where

import Test.HUnit
import Text.RawString.QQ
import System.Exit

multilineUnixNewlines :: String
multilineUnixNewlines = [r|FOO
BAR|]

multilineWindowsNewlines :: String
multilineWindowsNewlines = [r|FOO
BAR|]

main :: IO ()
main = defaultMain $ test [
  "Windows newlines" ~: (multilineUnixNewlines ~=? multilineWindowsNewlines)
  ]

defaultMain :: Test -> IO ()
defaultMain t = do
  cnts <- runTestTT t
  case failures cnts + errors cnts of
    0 -> exitWith $ ExitSuccess
    n -> exitWith $ ExitFailure n
