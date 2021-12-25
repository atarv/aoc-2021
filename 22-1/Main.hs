#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "text parsec containers"
 --ghc-options -Wall
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment
import           Data.List
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T
import           Text.Parsec.Combinator
import           Text.Parsec.Char
import           Text.Parsec.Prim
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

type Parser a = Parsec Text () a

type Coordinate = (Int, Int, Int)
type Range = (Int, Int)
type Cube = (Range, Range, Range)
data Step = Step Bool Cube deriving (Show, Eq)
type Reactor = Map Coordinate Bool

maxX, maxY, maxZ :: Int
maxX = 50; maxY = 50; maxZ = 50

minX, minY, minZ :: Int
minX = -50; minY = -50; minZ = -50

step :: Parser Step
step = do
    onoff <- try (Step True <$ string "on ") <|> (Step False <$ string "off ")
    onoff <$> cube
  where
    cube :: Parser Cube
    cube = (,,) <$> range <* char ',' <*> range <* char ',' <*> range
    range :: Parser Range
    range = (,) <$ letter <* char '=' <*> int <* string ".." <*> int
    int :: Parser Int
    int = do
        sign <- option ' ' (char '-')
        num  <- many1 digit
        pure (read $ sign : num)

parseSteps :: String -> Text -> [Step]
parseSteps source input = case parse (step `sepEndBy` newline) source input of
    Right steps -> steps
    Left  err   -> error $ show err

cubeToCoords :: Cube -> [Coordinate]
cubeToCoords ((xLow, xHigh), (yLow, yHigh), (zLow, zHigh)) =
    [ (x, y, z)
    | x <- [(max xLow minX) .. (min xHigh maxX)]
    , y <- [(max yLow minY) .. (min yHigh maxY)]
    , z <- [(max zLow minZ) .. (min zHigh maxZ)]
    ]

runStep :: Step -> Reactor -> Reactor
runStep (Step state cube) reactor = Map.differenceWith
    (\_ newState -> Just newState)
    reactor
    cubeMap
    where cubeMap = Map.fromList $ zip (cubeToCoords cube) (repeat state)

runSteps :: [Step] -> Reactor -> Reactor
runSteps steps initial = foldl' (flip runStep) initial steps

countOnCubes :: Reactor -> Int
countOnCubes = Map.size . Map.filter id

main :: IO ()
main = do
    [inputPath] <- getArgs
    steps       <- parseSteps inputPath <$> T.readFile inputPath
    let initial = Map.fromList $ zip
            (cubeToCoords ((minZ, maxX), (minY, maxY), (minZ, maxZ)))
            (repeat False)
        booted = runSteps steps initial
    mapM_ print steps
    print $ countOnCubes booted
