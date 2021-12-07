#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split vector vector-algorithms"
 --ghc-options -Wall
-}
module Main where

import           System.Environment
import           Data.List.Split
import           Data.Vector.Unboxed            ( Vector )
import qualified Data.Vector.Unboxed           as V
import qualified Data.Vector.Algorithms.Intro  as V

type Position = Int
type Positions = Vector Position

solvePosition :: Double -> Double -> Positions -> Position
solvePosition median avg positions =
    searchUntilWorse positions (round median) (median <= avg)

searchUntilWorse :: Positions -> Position -> Bool -> Position
searchUntilWorse positions start toRight =
    let cost  = calculateFuelCost positions start
        next  = (if toRight then succ else pred) start
        cost' = calculateFuelCost positions next
    in  if cost <= cost' then start else searchUntilWorse positions next toRight

calculateFuelCost :: Positions -> Position -> Int
calculateFuelCost positions target = V.foldl'
    (\acc pos -> acc + (cost (abs (target - pos))))
    0
    positions
    where cost delta = delta * (delta + 1) `div` 2

main :: IO ()
main = do
    [inputPath]   <- getArgs
    crabPositions <-
        V.modify V.sort
        .   V.fromList
        .   fmap read
        .   splitOn ","
        <$> readFile inputPath :: IO (Vector Int)
    let avg = fromIntegral (V.sum crabPositions)
            / fromIntegral (V.length crabPositions)
        median =
            fromIntegral (crabPositions V.! (V.length crabPositions `div` 2))
        targetPosition = solvePosition median avg crabPositions
        solution       = calculateFuelCost crabPositions targetPosition
    print solution
