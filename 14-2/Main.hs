#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "containers text"
 --ghc-options -Wall
-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where

import           System.Environment
import           Data.List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Bifunctor

type Instructions = Map Text Text
type PairCounts = Map Text Int

parseInput :: Text -> (PairCounts, Instructions)
parseInput input =
    let
        toPair (a : b : _) = (a, b)
        (template, instructionLines) =
            bimap head tail $ span (/= "") $ T.lines input
        instructions = fmap (toPair . T.splitOn " -> ") instructionLines
        pairs = fmap (T.take 2) $ filter ((> 1) . T.length) $ T.tails template
    in
        (Map.fromListWith (+) $ fmap (, 1) pairs, Map.fromList instructions)

step :: Instructions -> PairCounts -> PairCounts
step instructions template = Map.foldlWithKey'
    (\pairMap pair count ->
        foldl' (\acc p -> addPair p count acc) pairMap (formNewPairs pair)
    )
    Map.empty
    template
  where
    addPair = Map.insertWith (+)
    formNewPairs pair =
        let insertion = instructions Map.! pair
        in  [T.head pair `T.cons` insertion, insertion <> T.tail pair]

elementCounts :: PairCounts -> Map Char Int
elementCounts = Map.foldlWithKey'
    (\counts pair count -> foldl'
        (\acc e -> Map.insertWith (+) e (count) acc)
        counts
        (T.unpack pair)
    )
    Map.empty

main :: IO ()
main = do
    [inputPath] <- getArgs
    input       <- T.readFile inputPath
    let (polymerTemplate, pairInsertionRules) = parseInput input
        iteration10 = iterate (step pairInsertionRules) polymerTemplate !! 40
        min'        = minimum $ Map.elems $ elementCounts iteration10
        max'        = maximum $ Map.elems $ elementCounts iteration10
    -- This is very handwavy but whatever works, right?
    print $ (max' - min') `div` 2 + 1
