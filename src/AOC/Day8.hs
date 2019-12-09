module AOC.Day8
    ( 
    ) where

import qualified Data.List as List
import Data.Ord

part1 :: Int -> Int -> String -> Int
part1 width height input = do
    let layers = getLayers width height input
    let lowest0sLayer = List.minimumBy (comparing (occurancesOfDigit 0)) layers
    occurancesOfDigit 1 lowest0sLayer * occurancesOfDigit 2 lowest0sLayer

part2 :: Int -> Int -> String -> String
part2 width height input = do
    let layers = getLayers width height input
    let layer = foldr applyNextLayer (replicate (width * height) 2) layers
    let layerSplitOnWidth = splitList width layer
    -- printable string call ghci with: putStr part2 ... to show newlines
    List.intercalate "\n" (map (List.intercalate "" . map show) layerSplitOnWidth)

getLayers width height input = do
    let pixels = splitIntoDigits input
    splitList (width * height) pixels

applyNextLayer :: [Int] -> [Int] -> [Int]
applyNextLayer previousLayer nextLayer =
    map getPixelColour (zip previousLayer nextLayer)

getPixelColour :: (Int, Int) -> Int
getPixelColour (previousPixel, nextLayerPixel)
    | previousPixel == 0 = 0
    | previousPixel == 1 = 1
    | otherwise = nextLayerPixel 

splitIntoDigits :: String -> [Int]
splitIntoDigits string =
    map (read . (:"")) string

occurancesOfDigit :: Int -> [Int] -> Int
occurancesOfDigit digit digits =
    length $ filter (==digit) digits

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n xs =
    let (ys, zs) = splitAt n xs
    in  ys : splitList n zs