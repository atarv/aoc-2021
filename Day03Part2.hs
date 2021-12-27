#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --ghc-options -Wall
-}
module Day03Part2 where

import           System.Environment
import           Data.List
import           Data.Bits

type BitCriteria = String -> Char

-- | Returns positive integer if there's more 1's than zeroes.
countBits :: String -> Integer
countBits = foldl' (\acc b -> acc + countBit b) 0
  where
    countBit '1' = 1
    countBit '0' = -1
    countBit n   = error $ "Expected binary number, got: " <> show n

mostCommonBit :: BitCriteria 
mostCommonBit bits = if countBits bits >= 0 then '1' else '0'

leastCommonBit :: BitCriteria
leastCommonBit bits = if mostCommonBit bits == '1' then '0' else '1'

bitVectorToInt :: [Bool] -> Integer
bitVectorToInt = foldl' (\acc b -> shiftL acc 1 + fromBoolean b) 0
  where
    fromBoolean True  = 1
    fromBoolean False = 0

main :: IO ()
main = do
    [inputPath]    <- getArgs
    diagnosticData <- lines <$> readFile inputPath
    let oxygenRating = findRatingByBitCriteria mostCommonBit diagnosticData
        co2Rating = findRatingByBitCriteria leastCommonBit diagnosticData
    print (oxygenRating, co2Rating, oxygenRating * co2Rating)

findRatingByBitCriteria :: BitCriteria -> [String] -> Integer
findRatingByBitCriteria criteria diagnostic = go diagnostic 0
  where
    go :: [String] -> Int -> Integer
    go diagnostic' bitPosition
        | length diagnostic' == 1
        = bitVectorToInt $ map (== '1') $ head diagnostic'
        | otherwise
        = let
              columns            = transpose diagnostic'
              criteriaBit         = criteria $ columns !! bitPosition
              matchingBitCriteria = filter
                  ((== criteriaBit) . (!! bitPosition))
                  diagnostic'
          in go matchingBitCriteria (bitPosition + 1)
