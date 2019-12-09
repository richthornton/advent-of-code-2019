module AOC.Day4
    ( 
    ) where

import qualified Data.List as List

part1 :: Int -> Int -> Int
part1 lowerBound upperBound = do
    let possibleNumbers = [lowerBound..upperBound]
    let subsequences = map digits possibleNumbers
    let filteredSubsequences = filter followsRules subsequences
    length filteredSubsequences

digits :: Int -> [Int]
digits = map (read . return) . show

followsRules :: [Int] -> Bool
followsRules subsequences
    | isIncreasing && hasConsecutive = True
    | otherwise = False
    where
        isIncreasing = isMonotonicallyIncreasingDigits 0 subsequences
        hasConsecutive = hasConsecutiveDoubleDigitOnly 0 subsequences

isMonotonicallyIncreasingDigits :: Int -> [Int] -> Bool
isMonotonicallyIncreasingDigits previousDigit digits
    | previousDigit > currentDigit = False
    | others == [] = True
    | otherwise = isMonotonicallyIncreasingDigits currentDigit others
    where
        (currentDigit:others) = digits

hasConsecutiveDoubleDigitOnly :: Int -> [Int] -> Bool
hasConsecutiveDoubleDigitOnly previousDigit digits
    | digits == [] = False
    | previousDigit == currentDigit && others == [] = True
    | others == [] = False
    | previousDigit == currentDigit && currentDigit == nextDigit = hasConsecutiveDoubleDigitOnly currentDigit (removeSequenceOfDigits currentDigit others)
    | previousDigit == currentDigit && currentDigit /= nextDigit = True
    | otherwise = hasConsecutiveDoubleDigitOnly currentDigit others
    where
        (currentDigit:others) = digits
        nextDigit = head others

removeSequenceOfDigits :: Int -> [Int] -> [Int]
removeSequenceOfDigits digit digits
        | rest == [] = rest
        | digit /= nextDigit = digits
        | otherwise = removeSequenceOfDigits digit rest
        where
            (nextDigit:rest) = digits

-- Part 1 calculation
-- hasConsecutiveDoubleDigit :: Int -> [Int] -> Bool
-- hasConsecutiveDoubleDigit previousDigit digits
--     | previousDigit == currentDigit = True
--     | others == [] = False
--     | otherwise = hasConsecutiveDoubleDigit currentDigit others
--     where
--         (currentDigit:others) = digits