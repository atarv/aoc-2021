#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --ghc-options -Wall
-}
module Day01Part2 where

import Prelude hiding (last)
import System.Environment

main :: IO ()
main = do
    [inputPath] <- getArgs
    input <- readFile inputPath
    let depths = fmap (read :: String -> Int) (lines input)
        incCount = countIncreasingWindowed 3 depths
    print incCount

countIncreasingWindowed :: Int -> [Int] -> Int
countIncreasingWindowed _ [] = 0
countIncreasingWindowed windowSize (x : xs) = go xs 0 x
  where 
    go ys total last 
        | length ys < windowSize = total
        | otherwise = let
                window = take windowSize ys
                windowSum = sum window
            in if windowSum > last 
                then go (tail ys) (total + 1) windowSum
                else go (tail ys) total windowSum
