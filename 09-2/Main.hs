#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "containers vector vector-algorithms"
 --ghc-options -Wall
-}
module Main where

import           System.Environment
import           Control.Monad
import           Data.Char
import           Data.Functor
import           Data.Maybe
import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Intro  as V

type HeightMap = Vector (Vector Int)
type Basin = Set (Int, Int) -- Locations that form a basin

safeIndex :: HeightMap -> Int -> Int -> Maybe Int
safeIndex hmap row col = join $ hmap V.!? row <&> (V.!? col)

-- Returns locations of low points instead of their heights
findLowPoints :: HeightMap -> Vector (Int, Int)
findLowPoints hmap = V.foldl1' (V.++) $ V.imap
    (\i row -> V.imapMaybe
        (\j point -> if isLowPoint i j point then Just (i, j) else Nothing)
        row
    )
    hmap
  where
    pickAdjacent :: Int -> Int -> [Int]
    pickAdjacent row col =
        let up    = safeIndex hmap (row + 1) col
            down  = safeIndex hmap (row - 1) col
            left  = safeIndex hmap row (col - 1)
            right = safeIndex hmap row (col + 1)
        in  catMaybes [up, down, left, right]
    isLowPoint :: Int -> Int -> Int -> Bool
    isLowPoint row col point = all (> point) (pickAdjacent row col)

findBasins :: HeightMap -> Vector (Int, Int) -> Vector Basin
findBasins hmap lowPoints = fmap findBasin lowPoints
  where
    findBasin :: (Int, Int) -> Basin
    findBasin location = go (Set.singleton location) location
    go basin (row, col) = case safeIndex hmap row col of
        Nothing -> basin
        Just 9  -> basin
        Just height ->
            let
                adjacent =
                    [ (row + 1, col)
                    , (row - 1, col)
                    , (row    , col - 1)
                    , (row    , col + 1)
                    ]
                newLocations = filter
                    (\loc ->
                        not (Set.member loc basin) && flowsTowards loc height
                    )
                    adjacent
            in
                foldl' (\b loc -> Set.union b (go b loc))
                       (Set.insert (row, col) basin)
                       newLocations
    -- Does location flow towards given height
    flowsTowards :: (Int, Int) -> Int -> Bool
    flowsTowards (row, col) point =
        maybe False (> point) $ safeIndex hmap row col

main :: IO ()
main = do
    [inputPath] <- getArgs
    heightMap   <-
        V.fromList
        .   fmap (V.fromList . fmap digitToInt)
        .   lines
        <$> readFile inputPath :: IO HeightMap
    let lowPoints  = findLowPoints heightMap
        basins     = findBasins heightMap lowPoints
        basinSizes = fmap length basins
        top3Sizes  = V.take 3 $ V.modify (V.sortBy (flip compare)) basinSizes
    print (V.product top3Sizes)
