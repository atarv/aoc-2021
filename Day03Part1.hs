#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --ghc-options -Wall
-}
module Day03Part1 where

import           System.Environment
import Data.List
import Data.Bits

-- | Returns positive integer if there's more 1's than zeroes.
countBits :: String -> Integer
countBits = foldl' (\acc b -> acc + countBit b) 0
  where
    countBit '1' = 1
    countBit '0' = -1
    countBit n = error $ "Expected binary number, got: " <> show n

bitVectorToInt :: [Bool] -> Integer
bitVectorToInt = foldl' (\acc b -> shiftL acc 1 + fromBoolean b) 0
  where
    fromBoolean True = 1
    fromBoolean False = 0

main :: IO ()
main = do
    [inputPath]  <- getArgs
    gammaRates <- lines <$> readFile inputPath
    let mcbs = (> 0) . countBits <$> transpose gammaRates
        lcbs = not <$> mcbs
        gammaRate = bitVectorToInt mcbs
        epsilonRate = bitVectorToInt lcbs
    print (gammaRate, epsilonRate, gammaRate * epsilonRate)
