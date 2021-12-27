#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split containers"
 --ghc-options -Wall
-}
module Day08Part1 where

import           System.Environment
import           Data.List.Split
import           Data.Bifunctor                 ( bimap )
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

toTuple :: [a] -> (a, a)
toTuple (a : b : _) = (a, b)
toTuple _ = error "Too few elements in list to form a tuple"

isEasyDigit :: String -> Bool
isEasyDigit segments = length segments `IntSet.member` IntSet.fromList [2,3,4,7]

parseInput :: String -> [([String], [String])]
parseInput = fmap (bimap words words . toTuple . splitOn "|") .   lines

main :: IO ()
main = do
    [inputPath]        <- getArgs
    input <- parseInput <$> readFile inputPath
    let easyCount = sum $ fmap (length . filter isEasyDigit) $ fmap snd input
    print easyCount
