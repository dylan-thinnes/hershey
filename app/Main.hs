{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Diagrams.Prelude
import Diagrams.TwoD.Image
import Diagrams.Backend.SVG.CmdLine
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P
import Data.Char (ord, isDigit, isSpace)
import Control.Monad (forM, when)
import Data.List (maximumBy, find)
import Data.Function (on)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, listToMaybe)
import System.IO (Handle, hGetContents, stdin, stderr, hPutStrLn)

main :: IO ()
main = do
  mbResults <- parseFromFile stdin
  case mbResults of
    Nothing -> hPutStrLn stderr "Input file could not be parsed."
    Just allResults ->
      --mainWith $ renderCharacterMap 30 allResults
      --mainWith $ renderCharacterMap 30 [c | i <- gothicSimplex, Just c <- [find ((i ==) . idx) allResults]]
      mainWith $ renderWrite allResults gothicSimplex (TextOptions (-1) 20 5)
        ["Hello, your name is Sam!", "I am 24 years old."]

  pure ()
