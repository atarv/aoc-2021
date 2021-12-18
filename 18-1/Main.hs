#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "text parsec"
 --ghc-options -Wall
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Text.Parsec.Combinator
import           Text.Parsec.Char
import           Text.Parsec.Prim

type Parser a = Parsec Text () a

data SnailfishNum
    = Regular Int
    | Pair SnailfishNum SnailfishNum
    deriving (Eq)

instance Show SnailfishNum where
    show (Regular n) = show n
    show (Pair a b ) = '[' : show a <> "," <> show b <> "]"

parseSnailfishNum :: Parser SnailfishNum
parseSnailfishNum = do
    regular <|> pair
  where
    regular = Regular . read <$> many1 digit
    pair    = between
        (char '[')
        (char ']')
        (Pair <$> parseSnailfishNum <* char ',' <*> parseSnailfishNum)

parseInput :: String -> Text -> [SnailfishNum]
parseInput sourcePath input =
    case parse (parseSnailfishNum `sepBy` newline) sourcePath input of
        Right nums -> nums
        Left  err  -> error $ show err

reduce :: SnailfishNum -> SnailfishNum
reduce num = undefined

type Apply = Maybe (Int -> Int)
-- Apply is a function that should be applied to the number just left of or
-- right of given pair (respective to tuple order).
-- If this function returns Nothing on snd of tuple, first reduction is not yet
-- applied.
applyFirstReduce
    :: (SnailfishNum, Maybe (Apply, Apply))
    -> Int
    -> (SnailfishNum, Maybe (Apply, Apply))
applyFirstReduce app@((Regular n, Nothing)) depth
    | n > 9     = (splitNum (Regular n), Just (Nothing, Nothing))
    | otherwise = app
-- Exploding pairs will always consist of two regular numbers
applyFirstReduce app@((Pair (Regular l) (Regular r)), Nothing) depth
    | depth == 4 = (Regular 0, Just (Just (+ l), Just (+ r)))
    | otherwise  = app
applyFirstReduce app@((Pair left right), Nothing) depth =
    let
        leftResult@(leftNum, leftApplies) =
            applyFirstReduce (left, Nothing) (depth + 1)
        rightResult@(rightNum, rightApplies) =
            applyFirstReduce (right, Nothing) (depth + 1)
    in
        case (leftApplies, rightApplies) of
            (Just (Just leftApply, Just rightApply), _) ->
                ( Pair leftNum (onRightmost rightApply right)
                , Just (Just leftApply, Nothing)
                )
            (Just (Nothing, Just rightApply), _) ->
                ( Pair leftNum (onRightmost rightApply right)
                , Just (Nothing, Nothing)
                )
            (Just (Nothing, Nothing), _) ->
                (Pair leftNum right, Just (Nothing, Nothing))
            (Nothing, Just (Just leftApply, Just rightApply)) ->
                ( Pair (onLeftmost leftApply left) rightNum
                , Just (Nothing, Just rightApply)
                )
            (Nothing, Just (Nothing, Just rightApply)) ->
                (Pair left rightNum, Just (Nothing, Just rightApply))
            (Nothing, Just (Nothing, Nothing)) ->
                (Pair left rightNum, Just (Nothing, Nothing))
            (Nothing, Just (applyLeft, applyRight)) -> undefined
            _ -> (Pair leftNum rightNum, Nothing)
applyFirstReduce app@((Pair left right), Just (Nothing, Nothing)) depth = app
applyFirstReduce ((Pair left right), Just (Just applyLeft, Just applyRight)) depth
    = ( Pair (onRightmost applyLeft left) (onLeftmost applyRight right)
      , Just (Nothing, Nothing)
      )

onLeftmost :: (Int -> Int) -> SnailfishNum -> SnailfishNum
onLeftmost fn (Regular x  ) = Regular $ fn x
onLeftmost fn (Pair left _) = onLeftmost fn left

onRightmost :: (Int -> Int) -> SnailfishNum -> SnailfishNum
onRightmost fn (Regular x   ) = Regular $ fn x
onRightmost fn (Pair _ right) = onLeftmost fn right

explode :: SnailfishNum -> SnailfishNum
explode num = undefined

splitNum :: SnailfishNum -> SnailfishNum
splitNum (Regular n)
    | n > 9     = Pair (Regular $ n `div` 2) (Regular $ n `div` 2 + 1)
    | otherwise = Regular n
splitNum num = num

main :: IO ()
main = do
    [inputPath]   <- getArgs
    snailfishNums <- parseInput inputPath <$> T.readFile inputPath
    mapM_ (putStrLn . show) snailfishNums
