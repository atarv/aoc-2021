#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split containers"
 --ghc-options -Wall
-}
module Day06Part2 where

import           System.Environment
import           Data.List
import           Data.List.Split
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

type State = Int
type FishStates = Map State Int -- fish states as key, count as value

updateFishStates :: FishStates -> FishStates
updateFishStates = Map.foldlWithKey' updateState Map.empty
  where
    updateState newStates 0 count =
        Map.insertWith (+) 8 count $ Map.insertWith (+) 6 count newStates
    updateState newStates state count =
        Map.insertWith (+) (state - 1) count newStates


main :: IO ()
main = do
    [inputPath]    <- getArgs
    fishStatesList <- fmap read . splitOn "," <$> readFile inputPath :: IO [Int]
    let fishStates = foldl'
            (\states state -> Map.insertWith (+) state 1 states)
            Map.empty
            fishStatesList
        afterDays = iterate updateFishStates fishStates !! 256
        fishCount = foldl' (+) 0 afterDays
    print fishCount
