#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split"
 --ghc-options -Wall
-}
{-# LANGUAGE TupleSections #-}
module Day04Part2 where

import           System.Environment
import           Data.List
import           Data.List.Split
import           Data.Maybe

type Board = [[(Int, Bool)]] -- Number as int and it's status (marked or not)

main :: IO ()
main = do
    [inputPath                   ] <- getArgs
    (numbersRow : _ : boardsChunk) <- lines <$> readFile inputPath
    let numbers = read <$> splitOn "," numbersRow :: [Int]
        boards :: [Board]
        boards =
            fmap (fmap ((, False) . read) . words) <$> splitOn [""] boardsChunk
        finalScore = solveLastWinningBoard numbers boards
    print finalScore

-- Calculates the final score of the board that wins last
solveLastWinningBoard :: [Int] -> [Board] -> Int
solveLastWinningBoard numbers boards = go numbers boards
  where
    go (number : ns) boards
        | all isWinningBoard updatedBoards
        -- fromJust is safe since `boards` contains at least 
        -- one non-winning board
        = let lastBoard    = fromJust $ find (not . isWinningBoard) boards
              -- mark last board again to get correct score
              [lastBoard'] = markNumbers number [lastBoard]
          in  finalScore number lastBoard'
        | otherwise = go ns updatedBoards
        where updatedBoards = markNumbers number boards

-- Mark numbers that match the target number
markNumbers :: Int -> [Board] -> [Board]
markNumbers target boards = fmap (markNumber target) boards
  where
    markNumber :: Int -> Board -> Board
    markNumber target board =
        fmap (fmap (\(n, marked) -> (n, marked || n == target))) board

isWinningBoard :: Board -> Bool
isWinningBoard board =
    let hasWinningRow    = or $ fmap (all snd) board
        hasWinningColumn = or $ fmap (all snd) (transpose board)
    in  hasWinningRow || hasWinningColumn

finalScore :: Int -> Board -> Int
finalScore finalNum board =
    let filterUnmarked = filter (not . snd) :: [(a, Bool)] -> [(a, Bool)]
        boardSum       = sum $ fmap sum $ fmap (fmap fst . filterUnmarked) board
    in  boardSum * finalNum
