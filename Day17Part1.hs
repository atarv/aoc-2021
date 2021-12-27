#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "text parsec"
 --ghc-options -Wall
 --optimize
-}
{-# LANGUAGE OverloadedStrings #-}
module Day17Part1 where

import           System.Environment
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Text.Parsec.Combinator  hiding ( between )
import           Text.Parsec.Char
import           Text.Parsec.Prim

type Parser a = Parsec Text () a
type Coordinate = (Int, Int)
type Velocity = (Int, Int)
type Bounds = ((Int, Int), (Int, Int)) -- X-bounds and Y-bounds respectively

parseTarget :: Parser Bounds
parseTarget = do
    _       <- string "target area: "
    xBounds <- parseBounds
    _       <- char ','
    _       <- try $ many (oneOf "\t \n")
    yBounds <- parseBounds
    pure (xBounds, yBounds)

parseBounds :: Parser (Int, Int)
parseBounds = do
    _     <- anyChar *> char '='
    left  <- read <$> parseInt
    _     <- count 2 (char '.')
    right <- read <$> parseInt
    pure (left, right)
    where parseInt = (<>) <$> option "" (string "-") <*> many digit

parseInput :: Text -> Bounds
parseInput input = case parse parseTarget "" input of
    Right bounds -> bounds
    Left  err    -> error $ show err

isWithin :: Coordinate -> Bounds -> Bool
isWithin (x, y) (xBounds, yBounds) = x `between` xBounds && y `between` yBounds
    where between n (a, b) = n <= max a b && n >= min a b

findHighestY :: Bounds -> Int
findHighestY bounds =
    let trajectories =
                -- Pruning these could make this program run faster
                fmap trajectory
                    $ [ ((x, y), (0, 0)) | x <- [1 .. 200], y <- [1 .. 300] ]
        valid = filter (any (`isWithin` bounds)) $ fmap (fmap snd) trajectories
    in  maximum $ maximum $ fmap (fmap snd) valid
  where
    trajectory :: (Velocity, Coordinate) -> [(Velocity, Coordinate)]
    trajectory velocity =
        takeWhile (not . outOfBounds . snd) $ iterate step velocity
    outOfBounds :: Coordinate -> Bool
    outOfBounds (_, y) =
        let (bound1, bound2) = snd bounds in y < min bound1 bound2

step :: (Velocity, Coordinate) -> (Velocity, Coordinate)
step ((xVel, yVel), (x, y)) = ((towards0 xVel, yVel - 1), (x + xVel, y + yVel))

center :: Bounds -> Coordinate
center ((xLow, xHigh), (yLow, yHigh)) =
    ((xLow + xHigh) `div` 2, (yLow + yHigh) `div` 2)

towards0 :: Int -> Int
towards0 n | n > 0     = n - 1
           | n < 0     = n + 1
           | otherwise = 0

main :: IO ()
main = do
    [inputPath] <- getArgs
    bounds      <- parseInput <$> T.readFile inputPath
    let highY = findHighestY bounds
    print highY
