#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "mtl"
 --ghc-options -Wall
-}
{-# LANGUAGE StrictData, RecordWildCards #-}
module Day21Part1 where

import           System.Environment
import           Control.Monad
import           Control.Monad.State.Strict

data Player = Player { score :: Int, position :: Int } deriving (Show, Eq)
data Game = Game
    { p1 :: Player
    , p2 :: Player
    , die :: Int
    , rolls :: Int
    , p1HasTurn :: Bool
    } deriving (Show, Eq)

parseInput :: String -> (Int, Int)
parseInput input =
    let (first : second : _) = lines input
        parse = read . drop (length "Player 1 starting position: ")
    in  (parse first, parse second)

roll :: State Game Int
roll = do
    value <- gets die
    n     <- gets rolls
    modify' (\s -> s { die = (value `mod` 100) + 1, rolls = n + 1 })
    pure value

updatePlayer :: Int -> Player -> Player
updatePlayer rollSum player@(Player {..}) = player
    { score    = score + newPosition
    , position = newPosition
    }
    where newPosition = (position + rollSum - 1) `mod` 10 + 1

mkGame :: Int -> Int -> Game
mkGame p1Position p2Position =
    Game (Player 0 p1Position) (Player 0 p2Position) 1 0 True

play :: State Game ()
play = do
    p1Turn <- gets p1HasTurn
    rolls  <- replicateM 3 roll
    if p1Turn
        then do
            player <- gets p1
            modify' (\s -> s { p1 = updatePlayer (sum rolls) player })
        else do
            player <- gets p2
            modify' (\s -> s { p2 = updatePlayer (sum rolls) player })
    p1Score <- gets (score . p1)
    p2Score <- gets (score . p2)
    if (max p1Score p2Score) >= 1000
        then pure ()
        else do
            modify' (\s -> s { p1HasTurn = not (p1HasTurn s) })
            play

main :: IO ()
main = do
    [inputPath]        <- getArgs
    (player1, player2) <- parseInput <$> readFile inputPath
    let init     = mkGame player1 player2
        endState = execState play init
        loserScore = min (score . p1 $ endState) (score . p2 $ endState)
    print $ (rolls endState) * loserScore
