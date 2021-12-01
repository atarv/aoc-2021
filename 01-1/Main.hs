#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --ghc-options -Wall
-}
module Main where

import Prelude hiding (last)
import System.Environment

main :: IO ()
main = do
    [inputPath] <- getArgs
    input <- readFile inputPath
    let depths = fmap (read :: String -> Int) (lines input)
        incCount = countIncreasing depths
    print incCount

countIncreasing :: [Int] -> Int
countIncreasing [] = 0
countIncreasing (x : xs) = go xs 0 x
    where 
        go [] total _ = total
        go (y : ys) total last = if y >= last 
            then go ys (total + 1) y 
            else go ys total y