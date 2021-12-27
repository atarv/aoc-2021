#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "containers mtl"
 --ghc-options -Wall
-}
{-# LANGUAGE TupleSections #-}
module Day11Part1 where

import           System.Environment
import           Data.List
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

type Location = (Int, Int)
type Octopuses = Map Location (Int, Bool)

constructMap :: [[Int]] -> Octopuses
constructMap lists =
    let indexed = zip [0 ..] (fmap (zip [0 ..]) lists) :: [(Int, [(Int, Int)])]
        byLocation = concatMap
            (\(row, xs) -> fmap (\(col, energy) -> ((row, col), energy)) xs)
            indexed
    in  fmap (, False) $ Map.fromAscList byLocation

step :: State Octopuses Int
step = do
    modify' incrementEnergyLevels
    locationsToFlash <- toFlash
    forM_ locationsToFlash flash
    flashedCount <- countFlashed
    resetEnergyLevels
    pure flashedCount
  where
    incrementEnergyLevels = fmap (\(energy, flashed) -> (energy + 1, flashed))
    flash :: Location -> State Octopuses ()
    flash location = do
        loc <- gets (Map.lookup location)
        case loc of
            Nothing   -> pure ()
            Just (energy, flashed) -> do
                let energy' = energy + 1
                    flashed' = energy' > 9
                modify'
                    (Map.adjust (const (energy', flashed')) location)
                when (not flashed && flashed') $ do
                    forM_ (neighbors location) flash
    toFlash :: State Octopuses [Location]
    toFlash = fmap fst . Map.toList <$> gets
        (Map.filter (\(energy, flashed) -> energy > 9 && not flashed))
    neighbors :: Location -> [Location]
    neighbors (row, col) =
        [ (x, y) | x <- [row - 1, row, row + 1], y <- [col - 1, col, col + 1] ]
            \\ [(row, col)]
    countFlashed      = gets (Map.size . Map.filter snd)
    resetEnergyLevels = modify'
        (fmap (\(energy, _) -> (if energy > 9 then 0 else energy, False)))

simulate :: Int -> State Octopuses Int
simulate iterations 
    | iterations <= 0 = pure 0
    | otherwise = do
        count  <- step
        count' <- simulate (iterations - 1)
        pure (count + count')

showMap :: Octopuses -> String
showMap octopuses =
    let
        byRow =
            groupBy (\a b -> (fst . fst) a == (fst . fst) b)
                $ Map.toList octopuses :: [[(Location, (Int, Bool))]]
    in  unlines $ fmap (concatMap (show . fst . snd)) byRow

main :: IO ()
main = do
    [inputPath] <- getArgs
    octopuses   <-
        constructMap
        .   fmap (fmap (\c -> read [c]))
        .   lines
        <$> readFile inputPath
    let (flashCount, finalState) = runState (simulate 100) octopuses
    print flashCount
