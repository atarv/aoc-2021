#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split containers"
 --ghc-options -Wall
-}
{-# LANGUAGE TupleSections #-}
module Main where

import           System.Environment
import           Data.List
import           Data.List.Split
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

type Coordinate = (Int, Int)
type Line = (Coordinate, Coordinate)
type PointMap = Map Coordinate Int

-- Range where first number is always less than or equal to second number
newtype Range = Range { unRange :: (Int, Int) }

mkRange :: Int -> Int -> Range
mkRange a b = Range (min a b, max a b)

rangeToList :: Range -> [Int]
rangeToList (Range (a, b)) = [a .. b]

fromCoordinates :: [Coordinate] -> Line
fromCoordinates (a : b : _) = (a, b)
fromCoordinates _ = error "Unexpected number of coordinates for a line"

parseCoordinate :: String -> Coordinate
parseCoordinate input = case splitOn "," input of
    [x, y] -> (read x, read y)
    _      -> error $ "failed to parse coordinate from: " <> input

isVertical :: Line -> Bool
isVertical ((x1, _), (x2, _)) = x1 == x2

isHorizontal :: Line -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

lineToCoordinates :: Line -> [Coordinate]
lineToCoordinates line@(start, end)
    | isHorizontal line = fmap (, snd start) $ rangeToList $ mkRange
        (fst start)
        (fst end)
    | isVertical line = fmap (fst start, ) $ rangeToList $ mkRange
        (snd start)
        (snd end)
    |
    -- Relying on the fact that input contains only horizontal, vertical and 
    -- diagonal lines
      otherwise = diagonalToList line
  where
    diagonalToList (start'@(x1, y1), end'@(x2, y2)) =
        (:) end' $ takeWhile (/= end') $ case (x1 <= x2, y1 <= y2) of
            (True , True ) -> iterate (\(a, b) -> (a + 1, b + 1)) start'
            (True , False) -> iterate (\(a, b) -> (a + 1, b - 1)) start'
            (False, True ) -> iterate (\(a, b) -> (a - 1, b + 1)) start'
            (False, False) -> iterate (\(a, b) -> (a - 1, b - 1)) start'

addToPoints :: PointMap -> Line -> PointMap
addToPoints points line = foldl'
    (\pts coord -> Map.insertWith (+) coord 1 pts)
    points
    (lineToCoordinates line)

main :: IO ()
main = do
    [inputPath] <- getArgs
    input       <- lines <$> readFile inputPath
    let lines' :: [Line]
        lines' =
            fmap (fromCoordinates . fmap parseCoordinate . splitOn "->") input
        pointsMap        = foldl' addToPoints Map.empty lines'
        overlappingCount = length $ Map.filter (> 1) pointsMap
    print overlappingCount