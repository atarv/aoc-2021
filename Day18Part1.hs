#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "text parsec"
 --ghc-options -Wall
-}
{-# LANGUAGE OverloadedStrings #-}
module Day18Part1 where

import           System.Environment
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T
import           Text.Parsec.Combinator
import           Text.Parsec.Char
import           Text.Parsec.Prim
import           Debug.Trace

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

repeatUntilNoChange :: Eq a => (a -> a) -> a -> a
repeatUntilNoChange fn val =
    let val' = fn val
    in  if val' == val then val' else repeatUntilNoChange fn val'

reduce :: SnailfishNum -> SnailfishNum
reduce num =
    let
        reduced = repeatUntilNoChange
            (\n -> fst $ applyFirstReduce (n, Nothing) 0)
            num
        (splitted, _) = applySplit (reduced, False)
        trace'        = trace
            (  "num    : "
            <> show num
            <> "\nreduced: "
            <> show reduced
            <> "\nsplittd: "
            <> show splitted
            )
    in
        if num == splitted then splitted else reduce splitted

type Apply = Maybe (Int -> Int)
-- Apply is a function that should be applied to the number just left of or
-- right of given pair (respective to tuple order).
-- If this function returns Nothing on snd of tuple, first reduction is not yet
-- applied.
applyFirstReduce
    :: (SnailfishNum, Maybe (Apply, Apply))
    -> Int
    -> (SnailfishNum, Maybe (Apply, Apply))
applyFirstReduce app@(Regular n, _) _ = app
-- Exploding pairs will always consist of two regular numbers
applyFirstReduce app@(Pair (Regular l) (Regular r), Nothing) depth
    | depth == 4 = (Regular 0, Just (Just (+ l), Just (+ r)))
    | otherwise  = app
applyFirstReduce (Pair left right, Nothing) depth =
    let
        (leftNum, leftApplies) = applyFirstReduce (left, Nothing) (depth + 1)
        (rightNum, rightApplies) =
            applyFirstReduce (right, Nothing) (depth + 1)
    in
        case (leftApplies, rightApplies) of
            (Just (Just leftApply, Just rightApply), _) ->
                ( Pair leftNum (onLeftmost rightApply right)
                , Just (Just leftApply, Nothing)
                )
            (Just (Nothing, Just rightApply), _) ->
                ( Pair leftNum (onLeftmost rightApply right)
                , Just (Nothing, Nothing)
                )
            (Just (Just leftApply, Nothing), _) ->
                (Pair leftNum right, Just (Just leftApply, Nothing))
            (Just (Nothing, Nothing), _) ->
                (Pair leftNum right, Just (Nothing, Nothing))
            (Nothing, Just (Just leftApply, Just rightApply)) ->
                ( Pair (onRightmost leftApply left) rightNum
                , Just (Nothing, Just rightApply)
                )
            (Nothing, Just (Nothing, Just rightApply)) ->
                (Pair left rightNum, Just (Nothing, Just rightApply))
            (Nothing, Just (Just leftApply, Nothing)) ->
                ( Pair (onRightmost leftApply left) rightNum
                , Just (Nothing, Nothing)
                )
            (Nothing, Just (Nothing, Nothing)) ->
                (Pair left rightNum, Just (Nothing, Nothing))
            _ -> (Pair left right, Nothing)
applyFirstReduce (Pair left right, Just (Just applyLeft, Just applyRight)) _ =
    ( Pair (onRightmost applyLeft left) (onLeftmost applyRight right)
    , Just (Nothing, Nothing)
    )
applyFirstReduce (Pair left right, Just (Just applyLeft, Nothing)) _ =
    (Pair (onRightmost applyLeft left) right, Just (Nothing, Nothing))
applyFirstReduce (Pair left right, Just (Nothing, Just applyRight)) _ =
    (Pair left (onLeftmost applyRight right), Just (Nothing, Nothing))
applyFirstReduce app _ = app

applySplit :: (SnailfishNum, Bool) -> (SnailfishNum, Bool)
applySplit (num@(Regular n), applied) | applied   = (num, applied)
                                      | n > 9     = (splitNum num, True)
                                      | otherwise = (num, False)
applySplit (num@(Pair left right), applied)
    | applied
    = (num, applied)
    | otherwise
    = let (left' , leftApplied ) = applySplit (left, applied)
          (right', rightApplied) = applySplit (right, applied)
      in  if leftApplied
              then (Pair left' right, leftApplied)
              else if rightApplied
                  then (Pair left right', rightApplied)
                  else (num, False)

onLeftmost :: (Int -> Int) -> SnailfishNum -> SnailfishNum
onLeftmost fn (Regular x      ) = Regular $ fn x
onLeftmost fn (Pair left right) = Pair (onLeftmost fn left) right

onRightmost :: (Int -> Int) -> SnailfishNum -> SnailfishNum
onRightmost fn (Regular x      ) = Regular $ fn x
onRightmost fn (Pair left right) = Pair left (onRightmost fn right)

splitNum :: SnailfishNum -> SnailfishNum
splitNum (Regular n)
    | n > 9     = Pair (Regular $ n `div` 2) (Regular $ n `div` 2 + (n `mod` 2))
    | otherwise = Regular n
splitNum num = num

add :: SnailfishNum -> SnailfishNum -> SnailfishNum
add leftNum rightNum = reduce $ Pair leftNum rightNum

magnitude :: SnailfishNum -> Int
magnitude (Regular n      ) = n
magnitude (Pair left right) = magnitude left * 3 + magnitude right * 2

main :: IO ()
main = do
    [inputPath]   <- getArgs
    snailfishNums <- parseInput inputPath <$> T.readFile inputPath
    let snailFishSum   = foldl1 add snailfishNums
        magnitudeOfSum = magnitude snailFishSum
    print magnitudeOfSum
