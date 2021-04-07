module Test.Main where

import App (app)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_, throwError)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Prelude (Unit, bind, discard, not, when, ($), (<>), (==))

main :: Effect Unit
main =
  launchAff_ do
    inSvg <- readTextFile UTF8 "test/inputs/oxford-colleges.svg"
    expected <- readTextFile UTF8 "test/outputs/oxford-colleges.txt"
    let
      res = app inSvg
    for_ res.warnings log
    case res.result of
      Left _ -> throwError $ error "couldn't parse input file"
      Right actual -> do
        when (not $ actual == expected) do
          writeTextFile UTF8 out actual
        throwError $ error $ "unexpected test output - written to: " <> out
        where
        out = "test/outputs/oxford-colleges-actual.txt"
