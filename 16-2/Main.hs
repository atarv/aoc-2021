#!/usr/bin/env stack
{- stack script
 --resolver lts-18.18
 --package "parsec"
 --ghc-options -Wall
-}
module Main where

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
data Op = Sum | Product | Minimum | Maximum | GT' | LT' | EQ' deriving (Show, Eq)
data Packet 
    = Literal Version Int 
    | Operator Version Length Op [Packet]
    deriving (Show, Eq)

mkOp :: Int -> Op
mkOp n = case n of
    0 -> Sum
    1 -> Product
    2 -> Minimum
    3 -> Maximum
    5 -> GT'
    6 -> LT'
    7 -> EQ'

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
        opType -> do -- Operator
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
            pure $ Operator version opLength (mkOp opType) opPackets
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
    getVersions (Operator version _ _ subs) = version + sumVersions subs

interpret :: Packet -> Int
interpret (Literal _ val) = val
interpret (Operator _ _ op subs) = case op of
        Sum -> sum $ fmap interpret subs
        Product -> product $ fmap interpret subs
        Minimum -> minimum $ fmap interpret subs
        Maximum -> maximum $ fmap interpret subs
        LT' -> let [a,b] = subs in fromEnum $ (interpret a) < (interpret b)
        GT' -> let [a,b] = subs in fromEnum $ (interpret a) > (interpret b)
        EQ' -> let [a,b] = subs in fromEnum $ (interpret a) == (interpret b)

main :: IO ()
main = do
    [inputPath] <- getArgs
    binary      <- hexToBin <$> readFile inputPath
    let result = parse parsePackets inputPath binary
    case result of
        Right [packet] -> do
            print $ interpret packet
        Right packets -> do
            print $ fmap interpret packets
        Left err -> do
            print err
