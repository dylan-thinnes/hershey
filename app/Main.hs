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

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

data Character = Character
  { idx :: !Int
  , leftBound :: !Int
  , rightBound :: !Int
  , instrs :: ![Instr]
  }
  deriving (Show, Eq)

data Instr
  = Move Int Int
  | LiftPen
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderCharacter :: Character -> Diagram B
renderCharacter Character{..} =
  let renderGroup group = fromVertices (map (\(x, y) -> p2 (fromIntegral x, -fromIntegral y)) group)
      unpaddedLetter = foldMap renderGroup (splitGroups [] instrs) # lw 0.4
  in
  withEnvelope
    (unpaddedLetter <>
      fromVertices [p2 (fromIntegral leftBound, 0), p2 (fromIntegral rightBound, 0)])
    unpaddedLetter

splitGroups :: [(Int, Int)] -> [Instr] -> [[(Int, Int)]]
splitGroups group (LiftPen:rest) = group : splitGroups [] rest
splitGroups group (Move x y:rest) = splitGroups ((x, y) : group) rest
splitGroups group [] = [group]

renderCharacterMap :: Double -> [Character] -> Diagram B
renderCharacterMap charSize characters = vsep 1 $ do
  row <- chunksOf (ceiling (sqrt (fromIntegral (length characters)))) characters
  pure $ hsep 1 $ do
    character <- row
    pure (withEnvelope (square charSize :: Diagram B) (renderCharacter character))
  where
  chunksOf n [] = []
  chunksOf n xs =
    let (start, rest) = splitAt n xs
     in start : chunksOf n rest

data TextOptions = TextOptions
  { justify :: Double
  , aboveBaseline :: Double
  , belowBaseline :: Double
  }
  deriving (Show, Eq, Ord)

renderWrite :: [Character] -> [Int] -> TextOptions -> [String] -> Diagram B
renderWrite chars idxs TextOptions{..} texts = vcat $ map renderLine texts
  where
    renderLine text =
      let line = hcat $ map renderCharacter $ pickWithIndex chars idxs text
          yBound = fromVertices [p2 (0, -belowBaseline), p2 (0, aboveBaseline)]
      in
      alignX justify (withEnvelope (line <> yBound) line)

pickWithIndex :: [Character] -> [Int] -> String -> [Character]
pickWithIndex chars idxs text =
  map (\c -> mapping !! (fromEnum c - 32)) text
  where
    mapping :: [Character]
    mapping =
      [ fromMaybe (error "indexBy: given index that does not exist.") mChar
      | targetIdx <- idxs
      , let mChar = find (\char -> targetIdx == idx char) chars
      ]

gothicSimplex :: [Int]
gothicSimplex = concat
  [ [699, 714, 717, 733, 719, 2271, 734, 731]
  , [721, 722, 2219, 725, 711, 724, 710, 720]
  , [700..709]
  , [712, 713, 2241, 726, 2242, 715, 2273]
  , [501..526]
  , [2223, 804, 2224, 2262, 999, 730]
  , [601..626]
  , [2225, 723, 2226, 2246, 718]
  ]

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parseFromFile :: Handle -> IO (Maybe [Character])
parseFromFile handle = do
  contents <- hGetContents handle
  pure $ fmap fst $ listToMaybe $ P.readP_to_S linesP contents

getInt :: ReadP Int
getInt = P.readS_to_P reads

linesP :: ReadP [Character]
linesP = do
  commands <- P.sepBy lineP (P.char '\n')
  P.many $ P.satisfy isSpace
  P.eof
  pure commands

readIntN :: Int -> ReadP Int
readIntN n = do
  src <- P.count n $ P.satisfy $ \x -> isDigit x || isSpace x
  case readMaybe src of
    Nothing -> P.pfail
    Just i -> pure i

lineP :: ReadP Character
lineP = do
  idx <- readIntN 5
  lexemeCount <- readIntN 3
  (leftBound, rightBound) <- lexemeP
  let lexemesLeft = lexemeCount - 1
  instrs <- P.count lexemesLeft instrP
  pure Character{..}

lexemeP :: ReadP (Int, Int)
lexemeP = do
  let rel c = ord c - ord 'R'
  first <- P.get
  second <- P.get
  pure (rel first, rel second)

instrP :: ReadP Instr
instrP = do
  (l1, l2) <- lexemeP
  if l1 == -50 && l2 == 0
     then pure LiftPen
     else pure $ Move l1 l2
