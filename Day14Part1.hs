#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split containers"
 --ghc-options -Wall
-}
module Day14Part1 where

import           System.Environment
import           Data.List
import           Data.List.Split
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

type Instructions = Map String String

parseInput :: String -> (String, Instructions)
parseInput input =
    let (template, instructionLines) = fmap tail $ span (/= "") $ lines input
        instructions                 = fmap (\(a : b : _) -> (a, b))
            $ fmap (splitOn " -> ") instructionLines
    in  (head template, Map.fromList instructions)

step :: Instructions -> String -> String
step instructions template = go "" template
  where
    go result (a : b : rest) = 
        let insert = head $ instructions Map.! (a : b : [])
        in go (result <> (a : insert : [])) (b : rest)
    go result end = result <> end

main :: IO ()
main = do
    [inputPath]                           <- getArgs
    (polymerTemplate, pairInsertionRules) <- parseInput <$> readFile inputPath
    let iteration10 = iterate (step pairInsertionRules) polymerTemplate !! 10
        groupSizes = fmap length $ group $ sort iteration10
        mostCommonCount = maximum groupSizes
        leastCommonCount = minimum groupSizes
    print (mostCommonCount - leastCommonCount)
