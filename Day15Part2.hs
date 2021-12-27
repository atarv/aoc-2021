#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "containers"
 --ghc-options -Wall
-}
{-# LANGUAGE TupleSections #-}
module Day15Part2 where

import           System.Environment
import           Data.Maybe
import           Data.List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

type Location = (Int, Int)
type Graph = Map Location Int

parseGraph :: String -> Graph
parseGraph input =
    let risks = fmap (\x -> read [x]) <$> lines input :: [[Int]]
    in  Map.fromList
            [ ((x, y), risk)
            | (y, row ) <- zip [0 ..] risks
            , (x, risk) <- zip [0 ..] row
            ]

neighbors :: Location -> [Location]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1)]

lowestRiskPath :: Graph -> Location -> Location -> Int
lowestRiskPath graph start end = snd $ fromJust $ go
    Set.empty
    (Set.singleton (start, 0))
  where
    go :: Set Location -> Set (Location, Int) -> Maybe (Location, Int)
    go visited toVisit = case Set.minView toVisit of
        Nothing -> Nothing
        Just ((location, cost), withoutLocation)
            | location == end -> Just (location, cost)
            | location `Set.member` visited -> go visited withoutLocation
            | otherwise -> go
                (Set.insert location visited)
                (foldl' (flip Set.insert) withoutLocation newLocations)
          where
            newLocations =
                [ (location', cost + locationCost)
                | (location', locationCost) <- mapMaybe
                    (\neighbor -> (neighbor, ) <$> Map.lookup neighbor graph)
                    (neighbors location)
                ]

enlarge :: Graph -> Graph
enlarge graph =
    let (maxX, maxY) = fst $ Map.findMax graph
        wrapTo1 x = if x > 9 then x - 9 else x
    in  Map.unions
            [ Map.mapKeys
                  (\(x, y) -> (x + xMod * (maxX + 1), y + yMod * (maxY + 1)))
                  (Map.map (\cost -> wrapTo1 (cost + xMod + yMod)) graph)
            | xMod <- [0 .. 4]
            , yMod <- [0 .. 4]
            ]

showGraph :: Graph -> String
showGraph graph =
    let coords = sortOn
            snd
            [ (x, y)
            | x <- [0 .. (fst $ fst $ Map.findMax graph)]
            , y <- [0 .. (snd $ fst $ Map.findMax graph)]
            ]
        grouped = groupBy (\a b -> snd a == snd b) coords
        lookup loc = graph Map.! loc
    in  unlines $ concatMap (show . lookup) <$> grouped

main :: IO ()
main = do
    [inputPath] <- getArgs
    graph       <- parseGraph <$> readFile inputPath
    let enlarged            = enlarge graph
        enlargedBottomRight = fst $ Map.findMax enlarged
        enlargedLowestRisk  = lowestRiskPath enlarged (0, 0) enlargedBottomRight
    print enlargedLowestRisk
