#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "split containers mtl"
 --ghc-options -Wall
-}
module Day12Part2 where

import           System.Environment
import           Data.Foldable
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq

type Cave = String
type Connection = (Cave, Cave)
type Graph = Map Cave [Cave]

toPair :: [String] -> (String, String)
toPair (a : b : _) = (a, b)
toPair _           = error "Not enough elements to fill a pair"

isBig :: Cave -> Bool
isBig = all isUpper

isEnd :: Cave -> Bool
isEnd = (== "end")

showPath :: Seq Cave -> String
showPath = intercalate "," . toList

findPaths :: Graph -> Set (Seq Cave)
findPaths graph = delve False Set.empty Seq.empty "start"
  where
    isInPath :: Cave -> Seq Cave -> Bool
    isInPath cave = isJust . Seq.elemIndexL cave
    delve :: Bool -> Set (Seq Cave) -> Seq Cave -> Cave -> Set (Seq Cave)
    delve smallVisited paths path cave = Set.unions
        $ fmap (go smallVisited paths (path Seq.|> cave)) (graph Map.! cave)
    go :: Bool -> Set (Seq Cave) -> Seq Cave -> Cave -> Set (Seq Cave)
    go smallVisited paths path cave
        | isBig cave = delve smallVisited paths path cave
        | isEnd cave = Set.insert (path Seq.|> cave) paths
        | cave `isInPath` path && (smallVisited || cave `elem` ["start", "end"]) = paths
        | otherwise = delve (smallVisited || cave `isInPath` path)
                            paths
                            path
                            cave

parseGraph :: String -> Graph
parseGraph input =
    let edges = toPair . splitOn "-" <$> lines input :: [Connection]
        swap (a, b) = (b, a)
        singleton        = (: [])
        firstDirection   = fmap (fmap singleton) edges
        reverseDirection = fmap (fmap singleton . swap) edges
    in  Map.unionWith (<>)
                      (Map.fromListWith (<>) firstDirection)
                      (Map.fromListWith (<>) reverseDirection)

main :: IO ()
main = do
    [inputPath] <- getArgs
    graph       <- parseGraph <$> readFile inputPath :: IO Graph
    let paths = findPaths graph
    print $ Set.size paths
