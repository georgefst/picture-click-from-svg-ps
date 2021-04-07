module Test.Main where

import Prelude
import App (app)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (error, throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)

main :: Effect Unit
main = do
  inSvg <- readTextFile UTF8 "test/inputs/oxford-colleges.svg"
  expected <- readTextFile UTF8 "test/outputs/oxford-colleges.txt"
  res <- app inSvg
  for_ res.warnings log
  case res.result of
    Left _ -> throwException $ error "couldn't parse input file"
    Right actual ->
      when (not $ actual == expected) do
        writeTextFile UTF8 out actual
        throwException $ error $ "unexpected test output - written to: " <> out
      where
      out = "test/outputs/oxford-colleges-actual.txt"
