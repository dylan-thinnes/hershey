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
import Data.Foldable (toList)
import Debug.Trace

main :: IO ()
main = do
  --lines <- ignoreEmptyLines . splitWhileMergingLongLines <$> getContents
  --results <- forM lines $ \line -> do
  --  let result = P.readP_to_S lineP line
  --  if null result
  --     then putStrLn $ "Couldn't parse: " ++ show line
  --     else putStrLn $ "Success! " ++ show result
  --  pure $ case [a | (a, "") <- result] of
  --           [] -> error $ "Error: " ++ show result
  --           [x] -> x
  --mainWith $ vsep 1 $ map (hsep 1) $ take 40 $ chunksOf 40 $ map (renderCommand . snd) results

  --lines <- ignoreEmptyLines . splitWhileMergingLongLines <$> getContents
  --results <- forM lines $ \line -> do
  --  let result = P.readP_to_S lineP line
  --  if null result
  --     then putStrLn $ "Couldn't parse: " ++ show line
  --     else pure ()

  --contents <- getContents
  --let allResults = fst $ head $ P.readP_to_S linesP contents
  --let results = allResults `indexBy` gothicSimplex
  --let layoutSquareSize = ceiling (sqrt (fromIntegral (length results)))
  --mainWith $ vsep 1 $
  --  map (\xs -> (alignedText 1 0.5 (show (fst (head xs))) # fontSize 5) ||| hcat (map (renderCommand . snd) xs)) $
  --    chunksOf layoutSquareSize results

  contents <- getContents
  let allResults = fst $ head $ P.readP_to_S linesP contents
  mainWith $ hsep 0.1 $ map renderCommand $ writeWith (map snd (allResults `indexBy` gothicSimplex)) "Hello World!"

  pure ()

writeWith :: [a] -> String -> [a]
writeWith asciiMapping text = map (\c -> asciiMapping !! (fromEnum c - 32)) text

indexBy :: [(Int, a)] -> [Int] -> [(Int, a)]
indexBy map idxs =
  [ (idx, a)
  | idx <- idxs
  , a <- toList (idx `lookup` map)
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

overlapEnvelope :: Diagram B -> Diagram B -> Diagram B
overlapEnvelope overlapper dia =
  setEnvelope (overlapper ^. envelope <> dia ^. envelope) dia

renderCommand :: Command -> Diagram B
renderCommand Command{..} =
  foldMap renderGroup (splitGroups [] instrs)
    # overlapEnvelope (rect (fromIntegral rightBound - fromIntegral leftBound) 30 :: Diagram B)
    -- # overlapEnvelope (rect 30 30 :: Diagram B)
    # lw 0.4
  where
    renderGroup group = fromVertices (map (\(x, y) -> p2 (fromIntegral x, -fromIntegral y)) group)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = let (start, rest) = splitAt n xs in start : chunksOf n rest

splitGroups :: [(Int, Int)] -> [Instr] -> [[(Int, Int)]]
splitGroups group (LiftPen:rest) = group : splitGroups [] rest
splitGroups group (Move x y:rest) = splitGroups ((x, y) : group) rest
splitGroups group [] = [group]

data Command = Command
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

linesP :: ReadP [(Int, Command)]
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

lineP :: ReadP (Int, Command)
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
  pure (idx, Command{..})

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
