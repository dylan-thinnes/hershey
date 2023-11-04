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
import Data.List (maximumBy)
import Data.Function (on)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Debug.Trace

main :: IO ()
main = do
  contents <- getContents
  let allResults = fst $ head $ P.readP_to_S linesP contents
  --mainWith $ renderCharacterMap 30 (fmap snd allResults)
  mainWith $
    hcat $ map renderCharacter $
      writeWithIndex allResults gothicSimplex "My name is Dylan!"

  pure ()

--------------------------------------------------------------------------------
-- Indexing via Maps
--------------------------------------------------------------------------------

writeWithIndex :: [(Int, Character)] -> [Int] -> String -> [Character]
writeWithIndex chars idxs text =
  let mapping = chars `indexBy` idxs
  in
  map (\c -> mapping !! (fromEnum c - 32)) text

indexBy :: [(Int, a)] -> [Int] -> [a]
indexBy chars idxs =
  [ fromMaybe
      (error "indexBy: given index that does not exist.")
      (idx `lookup` chars)
  | idx <- idxs
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

data Character = Character
  { idx :: Int
  , leftBound :: Int
  , rightBound :: Int
  , instrs :: [Instr]
  }
  deriving (Show, Eq)

data Instr
  = Move Int Int
  | LiftPen
  deriving (Show, Eq)

getInt :: ReadP Int
getInt = P.readS_to_P reads

linesP :: ReadP [(Int, Character)]
linesP = do
  commands <- P.sepBy lineP (P.many (P.char '\n'))
  P.many $ P.satisfy isSpace
  P.eof
  pure commands

readIntN :: Int -> ReadP Int
readIntN n = do
  src <- P.count n $ P.satisfy $ \x -> isDigit x || isSpace x
  case readMaybe src of
    Nothing -> P.pfail
    Just i -> pure i

lineP :: ReadP (Int, Character)
lineP = do
  idx <- readIntN 5
  lexemeCount <- readIntN 3
  (leftBound, rightBound) <- lexemeP
  let lexemesLeft = lexemeCount - 1
  instrs <-
    if lexemesLeft == 31
      then do
        instrs <- P.count lexemesLeft instrP
        P.char '\n'
        pure instrs
      else do
        let groupSizes
              | lexemesLeft <= 31 = [lexemesLeft]
              | otherwise =
                  let (a, b) = (lexemesLeft - 31) `divMod` 36
                  in
                  filter (/= 0) $ [31] ++ replicate a 36 ++ [b]
        groups <- flip traverse groupSizes $ \groupSize -> do
          group <- P.count groupSize instrP
          P.char '\n'
          pure group
        pure (concat groups)
  pure (idx, Character{..})

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
