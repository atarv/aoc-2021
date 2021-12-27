#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split containers mtl"
 --ghc-options -Wall
-}
{-# LANGUAGE TupleSections #-}
module Day13Part2 where

import           System.Environment
import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

type Dot = (Int, Int)
type Paper = Set Dot
data Fold = X Int | Y Int deriving (Show, Eq)

readFold :: String -> Fold
readFold s =
    let (axis : _ : num) = drop (length "fold along ") s
    in  case axis of
            'x' -> X (read num)
            'y' -> Y (read num)
            _   -> error $ "Could not read fold instruction: " <> s

parseInstructions :: String -> (Paper, [Fold])
parseInstructions input =
    let lines'                = lines input :: [String]
        (dotLines, foldLines) = fmap tail $ span (/= "") lines'
        braces s = "(" <> s <> ")"
        dots  = fmap (read . braces) dotLines :: [Dot]
        folds = fmap readFold foldLines
    in  (Set.fromList dots, folds)

foldAlong :: Paper -> Fold -> Paper
foldAlong paper (X x) =
    let (left, right) =
                Set.partition ((< x) . fst) $ Set.filter ((/= x) . fst) paper
        translated = Set.map (\(x', y') -> (x + (x - x'), y')) right
    in  Set.union left translated
foldAlong paper (Y y) =
    let (up, down) =
                Set.partition ((< y) . snd) $ Set.filter ((/= y) . snd) paper
        translated = Set.map (\(x', y') -> (x', y + (y - y'))) down
    in  Set.union up translated

showPaper :: Paper -> String
showPaper paper =
    let maxX = maximum $ fmap fst $ Set.toList paper
        maxY = maximum $ fmap snd $ Set.toList paper
        toChar b = if b then '#' else ' '
    in  unlines $ fmap
            (\y -> fmap (toChar . flip Set.member paper . (, y)) [0 .. maxX])
            [0 .. maxY]

main :: IO ()
main = do
    [inputPath]   <- getArgs
    (paper, folds) <-
        parseInstructions <$> readFile inputPath :: IO (Paper, [Fold])
    let folded = foldl' foldAlong paper folds
    putStrLn $ showPaper folded
