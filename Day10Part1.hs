#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --ghc-options -Wall
-}
module Day10Part1 where

import           System.Environment
import           Data.Maybe

isOpen :: Char -> Bool
isOpen = flip elem "([{<"

isMatching :: Char -> Char -> Bool
isMatching a b = case (a, b) of
    ('(', ')') -> True
    ('[', ']') -> True
    ('{', '}') -> True
    ('<', '>') -> True
    _          -> False

syntaxScore :: Char -> Int
syntaxScore c = case c of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _   -> error $ "unexpected character '" <> [c] <> "'"

validate :: String -> Maybe Int
validate ""       = Nothing
validate (x : xs) = go [x] xs
  where
    go [] [] = Nothing
    go [] (c : chunks) =
        if isOpen c then go [c] chunks else Just $ syntaxScore c
    go (s : stack) (c : chunks) = if isOpen c
        then go (c : s : stack) chunks
        else if isMatching s c then go stack chunks else Just (syntaxScore c)
    go _ _ = Nothing

main :: IO ()
main = do
    [inputPath] <- getArgs
    lines'      <- lines <$> readFile inputPath :: IO [String]
    let syntaxScoreTotal = sum $ catMaybes $ fmap validate lines'
    print syntaxScoreTotal

