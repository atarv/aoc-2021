#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "parsec"
 --ghc-options -Wall
-}
module Day16Part1 where

import           System.Environment
import           Data.Bits
import           Data.List
import           Text.Parsec.Prim
import           Text.Parsec.Combinator
import           Text.Parsec.Char

type Parser a
    = Parsec String  -- Stream type
                    ()  -- User defined state type
                       a -- Result type

type Version = Int
data Length = LengthBits Int | LengthSubPackets Int deriving (Show, Eq)
data Packet 
    = Literal Version Int 
    | Operator Version Length [Packet]
    deriving (Show, Eq)

hexToBin :: String -> String
hexToBin = concatMap (fromHex)
  where
    fromHex c = case c of
        '0' -> "0000"
        '1' -> "0001"
        '2' -> "0010"
        '3' -> "0011"
        '4' -> "0100"
        '5' -> "0101"
        '6' -> "0110"
        '7' -> "0111"
        '8' -> "1000"
        '9' -> "1001"
        'A' -> "1010"
        'B' -> "1011"
        'C' -> "1100"
        'D' -> "1101"
        'E' -> "1110"
        'F' -> "1111"
        _   -> error $ "Unexpected character: " <> [c]

binToInt :: String -> Int
binToInt = foldl' (\acc b -> shiftL acc 1 + fromChar b) 0
  where
    fromChar '0' = 0
    fromChar '1' = 1
    fromChar _   = error "Unexpected character while converting to int"

binDigit :: Parser Char
binDigit = oneOf "01"

parseNBitsToInt :: Int -> Parser Int
parseNBitsToInt nBits = binToInt <$> count nBits binDigit

parsePackets :: Parser [Packet]
parsePackets = many (try parsePacket) <* many binDigit <* eof

parsePacket :: Parser Packet
parsePacket = do
    version    <- parseNBitsToInt 3
    packetType <- parseNBitsToInt 3
    case packetType of
        4 -> do -- Literal
            parts <- manyTill literalPart (lookAhead literalEnd)
            end   <- literalEnd
            let literalBits = concat parts <> end
            pure $ Literal version (binToInt literalBits)
        _ -> do -- Operator
            opLength  <- parseLength
            opPackets <- case opLength of
                LengthBits nBits -> do
                    packetBits <- count nBits binDigit
                    let
                        subResult = parse (many (try parsePacket))
                                          "sub packet"
                                          packetBits
                    case subResult of
                        Right packets -> pure packets
                        Left err ->
                            fail
                                $  "Failed to parse sub packet "
                                <> show err
                                <> " sub packet: "
                                <> packetBits
                LengthSubPackets nPackets -> count nPackets parsePacket
            pure $ Operator version opLength opPackets
  where
    literalPart = char '1' *> count 4 binDigit
    literalEnd  = char '0' *> count 4 binDigit
    parseLength = do
        lenType <- binDigit
        case lenType of
            '0' -> LengthBits <$> parseNBitsToInt 15
            '1' -> LengthSubPackets <$> parseNBitsToInt 11

sumVersions :: [Packet] -> Int
sumVersions = sum . fmap getVersions
  where
    getVersions (Literal version _      ) = version
    getVersions (Operator version _ subs) = version + sumVersions subs

main :: IO ()
main = do
    [inputPath] <- getArgs
    binary      <- hexToBin <$> readFile inputPath
    let result = parse parsePackets inputPath binary
    case result of
        Right packets -> do
            print $ sumVersions packets
        Left err -> do
            print err
