module AOC.Day3Spec where

import SpecHelper

import Data.Sequence (fromList)

spec :: Spec
spec =
    describe "day3Part1" $ do
        context "parse input with 'R75'" $
            it "should return [(R,75)]" $
                parseInput "R75" `shouldBe` [('R',75)]

        context "parse input with 'R75,D30,R83,U83,L2'" $
            it "should return [(R,75),(D,30),(R,83),(U,83),(L,2)]" $
                parseInput "R75,D30,R83,U83,L2" `shouldBe` [('R',75),('D',30),('R',83),('U',83),('L',2)]
        
        context "inputs 'R75,D30,R83,U83,L12,D49,R71,U7,L72' and 'U62,R66,U55,R34,D71,R55,D58,R83" $
            it "should return 159" $
                getMinimumManhattenDistance "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"`shouldBe` 159

        context "inputs 'R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51' and 'U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" $
            it "should return 135" $
                getMinimumManhattenDistance "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"`shouldBe` 135

        context "getFewestCombinedSteps inputs 'R75,D30,R83,U83,L12,D49,R71,U7,L72' and 'U62,R66,U55,R34,D71,R55,D58,R83" $
            it "should return 610" $
                getFewestCombinedSteps "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"`shouldBe` 610

        context "getFewestCombinedSteps inputs 'R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51' and 'U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" $
            it "should return 410" $
                getFewestCombinedSteps "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"`shouldBe` 410

main :: IO ()
main = hspec spec