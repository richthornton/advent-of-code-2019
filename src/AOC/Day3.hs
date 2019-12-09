{-# LANGUAGE TupleSections #-}

module AOC.Day3
    ( parseInput
    , getMinimumManhattenDistance
    ) where

import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

getFewestCombinedSteps :: String -> String -> Int
getFewestCombinedSteps path1 path2 = do
    let (points1, points2) = getPoints path1 path2
    let crossingPoints = getCrossingPoints path1 path2
    let stepsToCrossings = map (findStepsToCrossing points1 points2) crossingPoints
    minimum stepsToCrossings

findStepsToCrossing :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int)  -> Int
findStepsToCrossing points1 points2 point =
    let
        steps1 = List.elemIndex point points1
        steps2 = List.findIndex (== point) points2
    in
        Maybe.fromMaybe 10000000000 steps1 + Maybe.fromMaybe 10000000000 steps2

getMinimumManhattenDistance :: String -> String -> Int
getMinimumManhattenDistance path1 path2 = do
    let crossingPoints = getCrossingPoints path1 path2
    let manhattenDistances = map (findManhattenDistance (0,0)) crossingPoints
    minimum manhattenDistances

getPoints :: String -> String -> ([(Int, Int)], [(Int, Int)])
getPoints path1 path2 =
    (convertInstructionsToPoints (parseInput path1), convertInstructionsToPoints (parseInput path2))

getCrossingPoints :: String -> String -> [(Int, Int)]
getCrossingPoints path1 path2 = do
    let (points1, points2) = getPoints path1 path2
    filter (\point -> point /= (0,0)) (findCrossingPoints points1 points2)

parseInput :: String -> [(Char, Int)]
parseInput wirePathInput =
    parseString wirePathInput []

parseString :: String -> [(Char, Int)] -> [(Char, Int)]
parseString string path
    | string == "" = path
    | otherwise = do
        let (segment, remainingString) = last (readP_to_S directionAndDistance string)
        parseString remainingString (path ++ [segment])

convertInstructionsToPoints :: [(Char, Int)] -> [(Int, Int)]
convertInstructionsToPoints =
    foldl (\points instruction -> points ++ convertInstructionToPoints (last points) instruction) [(0,0)]

findCrossingPoints :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
findCrossingPoints firstPoints secondPoints =
    Set.toList $ Set.intersection (Set.fromList firstPoints) (Set.fromList secondPoints)

findManhattenDistance :: (Int, Int) -> (Int, Int) -> Int
findManhattenDistance point1 point2 = 
    let (x1, y1) = point1
        (x2, y2) = point2
    in
        abs (x1 - x2) + abs (y1 - y2)

convertInstructionToPoints :: (Int, Int) -> (Char, Int) -> [(Int, Int)]
convertInstructionToPoints initialPoint instruction
        | direction == 'R' = filterInitial $ map (, initialY) [initialX..(initialX + steps)]
        | direction == 'L' = filterInitial $ map (, initialY) [initialX,(initialX - 1)..(initialX - steps)]
        | direction == 'U' = filterInitial $ map (initialX, ) [initialY..(initialY + steps)]
        | direction == 'D' = filterInitial $ map (initialX, ) [initialY,(initialY - 1)..(initialY - steps)]
            where
                (direction, steps) = instruction
                (initialX, initialY) = initialPoint
                filterInitial = filter (/= initialPoint)

directionAndDistance :: ReadP (Char, Int)
directionAndDistance = do
    dir <- direction
    dist <- numbers 1 <|> numbers 2 <|> numbers 3 <|> numbers 4
    commaOrEnd <- option Nothing (fmap Just (satisfy (\char -> elem char ","))) -- thrown away
    return (dir, dist)

direction :: ReadP Char
direction =
    satisfy isDirection

isDirection :: Char -> Bool
isDirection char =
    char `elem` "RULD"

numbers :: Int -> ReadP Int
numbers digits = do
    parse <- count digits digit
    return (read parse)

digit :: ReadP Char
digit =
    satisfy (\char -> char >= '0' && char <= '9')