#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split"
 --ghc-options -Wall
-}
module Main where

import           System.Environment
import           Data.List
import           Data.List.Split

updateFishStates :: [Int] -> [Int]
updateFishStates = concatMap update
  where update state
            | state == 0 = [6, 8]
            | otherwise = [state - 1]

main :: IO ()
main = do
    [inputPath] <- getArgs
    fishStates  <- fmap read . splitOn "," <$> readFile inputPath :: IO [Int]
    let afterDays = iterate updateFishStates fishStates !! 80
    print $ length afterDays
