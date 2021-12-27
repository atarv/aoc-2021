-- #!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "vector"
 --ghc-options -Wall
-}
module Day09Part1 where

import           System.Environment
import           Control.Monad
import           Data.Char
import           Data.Functor
import           Data.Maybe
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

type HeightMap = Vector (Vector Int)

findLowPoints :: HeightMap -> Vector Int
findLowPoints hmap = V.foldl1' (V.++)
    $ V.imap (\i row -> V.ifilter (\j point -> isLowPoint i j point) row) hmap
  where
    pickAdjacent :: Int -> Int -> [Int]
    pickAdjacent row col =
        let safeIndex :: Int -> Int -> Maybe Int
            safeIndex row col = join $ hmap V.!? row <&> (V.!? col)
            up    = safeIndex (row + 1) col
            down  = safeIndex (row - 1) col
            left  = safeIndex row (col - 1)
            right = safeIndex row (col + 1)
        in  catMaybes [up, down, left, right]
    isLowPoint :: Int -> Int -> Int -> Bool
    isLowPoint row col point = all (> point) (pickAdjacent row col)

main :: IO ()
main = do
    [inputPath] <- getArgs
    heightMap   <-
        V.fromList
        .   fmap (V.fromList . fmap digitToInt)
        .   lines
        <$> readFile inputPath :: IO HeightMap
    let lowPoints  = findLowPoints heightMap
        riskValues = fmap (+ 1) lowPoints
    print (V.sum riskValues)
