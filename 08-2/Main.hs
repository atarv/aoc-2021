#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split containers"
 --ghc-options -Wall
-}
module Main where

import           System.Environment
import           Data.List
import           Data.List.Split
import           Data.Bifunctor                 ( bimap )
import qualified Data.IntSet                   as IntSet
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe

type Segments = Set Char

toTuple :: [a] -> (a, a)
toTuple (a : b : _) = (a, b)
toTuple _           = error "Too few elements in list to form a tuple"

-- | Easy digits are those that can be distinguished from other digits by how
-- many segments they use
isEasyDigit :: Segments -> Bool
isEasyDigit segments =
    length segments `IntSet.member` IntSet.fromList [2, 3, 4, 7]

segmentsToEasyDigit :: Segments -> Char
segmentsToEasyDigit segments
    | length segments == 2 = '1'
    | length segments == 3 = '7'
    | length segments == 4 = '4'
    | length segments == 7 = '8'
    | otherwise            = error "Segments didn't form an easy digit"

-- | Swap map's keys and values
flipMap :: Ord b => Map a b -> Map b a
flipMap m = Map.fromList [ (v, k) | (k, v) <- Map.toList m ]

buildSegmentMapping :: [Segments] -> Map Segments Char
buildSegmentMapping segmentsList = initial `Map.union` solvedDigits
  where
    (easyDigits, otherDigits) =
        partition isEasyDigit segmentsList :: ([Segments], [Segments])
    initial = foldl'
        (\mapping segments ->
            Map.insert segments (segmentsToEasyDigit segments) mapping
        )
        Map.empty
        easyDigits
    easyToSegments =
        fromJust . flip Map.lookup (flipMap initial) :: Char -> Segments
    easyDigitsInOrder = fmap easyToSegments ['1', '4', '7', '8']
    intersectionLengths a bs = fmap (length . Set.intersection a) bs
    solveDigit segments =
        case intersectionLengths segments easyDigitsInOrder of
            -- Got these numbers by calculating them from the example
            [1, 2, 2, 5] -> '2'
            [2, 3, 3, 5] -> '3'
            [1, 3, 2, 5] -> '5'
            [2, 4, 3, 6] -> '9'
            [1, 3, 2, 6] -> '6'
            [2, 3, 3, 6] -> '0'
            _ -> error "This shouldn't happen since all digits are handled"
    solvedDigits =
        Map.fromList $ fmap (\segs -> (segs, solveDigit segs)) otherDigits
solveRow :: ([Segments], [Segments]) -> Int
solveRow (segments, output) = read $ fmap toDigit output
  where
    mapping = buildSegmentMapping segments
    toDigit :: Segments -> Char
    toDigit = fromJust . flip Map.lookup mapping

parseInput :: String -> [([Segments], [Segments])]
parseInput =
    fmap (bimap (parseSegments) (parseSegments) . toTuple . splitOn "|") . lines
    where parseSegments = fmap Set.fromList . words

main :: IO ()
main = do
    [inputPath] <- getArgs
    input       <- parseInput <$> readFile inputPath
    let outputSum = sum $ fmap solveRow input
    print outputSum
