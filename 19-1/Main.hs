#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "text parsec"
 --ghc-options -Wall
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T
import           Text.Parsec.Combinator
import           Text.Parsec.Char
import           Text.Parsec.Prim
import           Debug.Trace

type Parser a = Parsec Text () a

type Position = (Int, Int, Int)
type Scanner = (Maybe Position, [Position])

parseNum :: Parser Int
parseNum = do
    sign <- option ' ' (char '-')
    digits <- many1 digit
    pure $ read (sign : digits)

position :: Parser Position
position = (,,) <$> parseNum <* char ',' <*> parseNum <* char ',' <*> parseNum

scanner :: Parser Scanner
scanner = do
    _ <- string "--- scanner " <* many1 digit <* string " ---\n"
    beacons <- position `sepEndBy` newline
    pure (Nothing, beacons)

parseScanners :: String -> Text -> [Scanner]
parseScanners source input = 
    case parse (scanner `sepEndBy` newline) source input of
        -- Position of first scanner is assumed to be origo
        Right ((_, beacons) : xs) -> (Just (0,0,0), beacons) : xs
        Left err -> error $ show err

main :: IO ()
main = do
    [inputPath]   <- getArgs
    scanners <- parseScanners inputPath <$> T.readFile inputPath
    mapM_ print scanners
