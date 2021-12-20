#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "array containers extra"
 --ghc-options -Wall
-}
module Main where

import           System.Environment
import           Data.Ord
import           Data.Bits
import           Data.List
import           Data.List.Extra
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Array.Unboxed             ( UArray )
import qualified Data.Array.Unboxed            as A
import           Debug.Trace

type Enhancement = UArray Int Char
type Image = Map (Int, Int) Char

parseInput :: String -> (Enhancement, Image)
parseInput input =
    let (enhancement : _ : image) = lines input
        image'                    = Map.fromList
            [ ((x, y), char)
            | (x, row ) <- zip [0 ..] image
            , (y, char) <- zip [0 ..] row
            ]
    in  (A.listArray (0, 511) enhancement, image')

enhance :: Int -> Enhancement -> Image -> Image
enhance n enhanceArr sourceImage =
    let (xs  , ys  ) = unzip $ Map.keys sourceImage
        (minX, maxX) = minMax xs
        (minY, maxY) = minMax ys
        getPixel pos = Map.findWithDefault (if even n then '.' else '#')
                                           pos
                                           sourceImage
    in  Map.fromList
            [ ((x, y), enhanced)
            | x <- [(minX - 1) .. (maxX + 1)]
            , y <- [(minY - 1) .. (maxY + 1)]
            , let enhanced =
                      enhanceArr A.! (binToInt $ (getPixel <$> adjacent (x, y)))
            ]

minMax :: (Bounded a, Ord a) => [a] -> (a, a)
minMax = foldl' (\(minX', maxX') x -> (min minX' x, max maxX' x))
                (maxBound, minBound)

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) =
    sort [ (x', y') | x' <- [x - 1, x, x + 1], y' <- [y - 1, y, y + 1] ]

binToInt :: String -> Int
binToInt = foldl' (\acc b -> shiftL acc 1 + fromChar b) 0
  where
    fromChar '.' = 0
    fromChar '#' = 1
    fromChar _   = error "Unexpected character while converting to int"

countLightedPixels :: Image -> Int
countLightedPixels = Map.size . Map.filter (== '#')

showImage :: Image -> String
showImage image =
    let (xs  , ys  ) = unzip $ Map.keys image
        (minX, maxX) = minMax xs
        (minY, maxY) = minMax ys
        getPixel pos = Map.findWithDefault '.' pos image
    in  unlines
            $ fmap (fmap snd)
            $ groupOn (fst . fst)
            $ sortOn fst
            $ [ ((x, y), c)
              | x <- [(minX) .. (maxX)]
              , y <- [(minY) .. (maxY)]
              , let c = getPixel (x, y)
              ]

main :: IO ()
main = do
    [inputPath]         <- getArgs
    (enhanceArr, image) <- parseInput <$> readFile inputPath
    let enhanced = (enhance 1 enhanceArr) . (enhance 0 enhanceArr) $ image
    print $ countLightedPixels enhanced
