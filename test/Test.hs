{-# LANGUAGE QuasiQuotes #-}

module Main
       where

import Control.Monad

import Test.HUnit
import Text.RawString.QQ

multilineUnixNewlines :: String
multilineUnixNewlines = [r|FOO
BAR|]

multilineWindowsNewlines :: String
multilineWindowsNewlines = [r|FOO
BAR|]

main :: IO ()
main = void . runTestTT . test $ [
  "Windows newlines" ~: (multilineUnixNewlines ~=? multilineWindowsNewlines)
  ]
