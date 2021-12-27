#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --ghc-options -Wall
-}
module Day02Part2 where

import           System.Environment

main :: IO ()
main = do
    [inputPath]  <- getArgs
    instructions <- parseInput <$> readFile inputPath
    let (depth, pos, _) = interpret instructions
    print (depth * pos)

parseInput :: String -> [(String, Integer)]
parseInput input = parseInstruction <$> instructions
  where
    instructions = fmap words $ lines input
    -- All cases are not defined because input is assumed correct
    parseInstruction (instruction : amount : _) =
        (instruction, read amount :: Integer)
    parseInstruction _ = undefined

interpret :: [(String, Integer)] -> (Integer, Integer, Integer)
interpret instructions = go instructions (0, 0, 0)
  where
    go []       state                  = state
    go (i : is) (depth, position, aim) = case i of
        ("forward", n) -> go is (depth + (aim * n), position + n, aim)
        ("down"   , n) -> go is (depth, position, aim + n)
        ("up"     , n) -> go is (depth, position, aim - n)
        _              -> error "unexpected instruction"
