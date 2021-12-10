#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --ghc-options -Wall
-}
module Main where

import           System.Environment
import           Data.Either
import           Data.List

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

autocompleteScore :: String -> Int
autocompleteScore stack = calculateScore $ autocomplete stack ""

calculateScore :: String -> Int
calculateScore = foldr (\c acc -> acc * 5 + charToScore c) 0

charToScore :: Char -> Int
charToScore c = case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _   -> error $ "Unexpected character " <> [c]

autocomplete :: String -> String -> String
autocomplete []          completion = completion
autocomplete (c : stack) completion = case c of
    '(' -> autocomplete stack (')' : completion)
    '[' -> autocomplete stack (']' : completion)
    '{' -> autocomplete stack ('}' : completion)
    '<' -> autocomplete stack ('>' : completion)
    _   -> error $ "Unexpected character " <> [c]

validate :: String -> Either Int Int
validate ""       = Right 0
validate (x : xs) = go [x] xs
  where
    go []    [] = Right 0
    go stack [] = Left $ autocompleteScore stack
    go [] (c : chunks) =
        if isOpen c then go [c] chunks else Right $ syntaxScore c
    go (s : stack) (c : chunks) = if isOpen c
        then go (c : s : stack) chunks
        else if isMatching s c then go stack chunks else Right (syntaxScore c)

main :: IO ()
main = do
    [inputPath] <- getArgs
    lines'      <- lines <$> readFile inputPath :: IO [String]
    let incomplete = lefts $ fmap validate lines'
        middle     = sort incomplete !! (length incomplete `div` 2)
    print middle

