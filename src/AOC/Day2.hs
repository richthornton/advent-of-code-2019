module AOC.Day2
    ( day2Part1, day2Part2, addPositions, calculateResult
    ) where

import Data.Foldable (toList)
import Data.Sequence (Seq, index, fromList, update, lookup)

day2Part1 :: [Int] -> Int
day2Part1 list = do
    let modifiedValue1 = update 1 12 (fromList list)
    let modifiedValue2 = update 2 2 modifiedValue1
    head (toList (workOnSequence' 0 modifiedValue2))

day2Part2 :: [Int] -> [Int]
day2Part2 list =
    toList (getOutput19690720 0 0 (fromList list))

getOutput19690720 :: Int -> Int -> Seq Int -> Seq Int
getOutput19690720 nounValue verbValue originalSequence = do
    let modifiedValue1 = update 1 nounValue originalSequence
    let modifiedValue2 = update 2 verbValue modifiedValue1
    let outputSequence = workOnSequence' 0 modifiedValue2
    let firstElement = head (toList (workOnSequence' 0 modifiedValue2))
    let nounAtMax = nounValue == 99
    let newNounValue = if nounAtMax then 0 else nounValue + 1
    let newVerbValue = if nounAtMax then verbValue + 1 else verbValue
    if firstElement == 19690720 then outputSequence else getOutput19690720 newNounValue newVerbValue originalSequence

calculateResult :: [Int] -> [Int]
calculateResult list =
    toList (workOnSequence' 0 (fromList list))

workOnSequence' :: Int -> Seq Int -> Seq Int
workOnSequence' currentIndex sequence
    | index sequence currentIndex == 1 = do
        let newIndex = currentIndex + 4
        let newValue = addPositions sequence (currentIndex + 1) (currentIndex + 2)
        let placeToPutIt = index sequence (currentIndex + 3)
        let updatedSequence = update placeToPutIt newValue sequence
        workOnSequence' newIndex updatedSequence
    | index sequence currentIndex == 2 = do
        let newIndex = currentIndex + 4
        let newValue = multiplyPositions sequence (currentIndex + 1) (currentIndex + 2)
        let placeToPutIt = index sequence (currentIndex + 3)
        let updatedSequence = update placeToPutIt newValue sequence
        workOnSequence' newIndex updatedSequence
    | index sequence currentIndex == 99 = sequence
    | otherwise = fromList [currentIndex]

addPositions :: Seq Int -> Int -> Int -> Int
addPositions sequence position1 position2 = do
    let index1 = index sequence position1
    let index2 = index sequence position2
    let value1 = index sequence index1
    let value2 = index sequence index2
    let newValue = value1 + value2
    newValue

multiplyPositions :: Seq Int -> Int -> Int -> Int
multiplyPositions sequence position1 position2 = do
    let index1 = index sequence position1
    let index2 = index sequence position2
    let value1 = index sequence index1
    let value2 = index sequence index2
    let newValue = value1 * value2
    newValue