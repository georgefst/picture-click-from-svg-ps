module Test.Main where

import Prelude
import App (app)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Test.QuickCheck (quickCheck')

main :: Effect Unit
main = do
  inSvg <- readTextFile UTF8 "test/inputs/oxford-colleges.svg"
  expected <- readTextFile UTF8 "test/outputs/oxford-colleges.txt"
  actual <- app { inSvg }
  let
    res = actual == expected
  when (not res) do
    let
      out = "test/outputs/oxford-colleges-actual.txt"
    log $ "unexpected test output - written to: " <> out
    writeTextFile UTF8 out actual
  quickCheck' 1 res
