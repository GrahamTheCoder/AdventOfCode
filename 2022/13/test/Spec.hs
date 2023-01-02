import Data.List
import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "areCorrectOrder" $ do
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      areCorrectOrder contents `shouldBe` [True, True, False, True, False, True, False, False]
    it "If the left integer is lower than the right integer, the inputs are in the right order" $ do
      areCorrectOrder "[1]\n[2]" `shouldBe` [True]
      areCorrectOrder "[1,1]\n[2,0]" `shouldBe` [True]
    it "If the left integer is higher than the right integer, the inputs are not in the right order" $ do
      areCorrectOrder "[2]\n[1]" `shouldBe` [False]
      areCorrectOrder "[2,1]\n[1,2]" `shouldBe` [False]
    it "the inputs are the same integer; continue checking the next part of the input" $ do
      areCorrectOrder "[1,1]\n[1,2]" `shouldBe` [True]
      areCorrectOrder "[1,1,1]\n[1,2,0]" `shouldBe` [True]
      areCorrectOrder "[1,2]\n[1,1]" `shouldBe` [False]
      areCorrectOrder "[1,2,1]\n[1,1,2]" `shouldBe` [False]
    it "If the left list runs out of items first, the inputs are in the right order" $ do
      areCorrectOrder "[[],2]\n[[1],1]" `shouldBe` [True]
    it "If the right list runs out of items first, the inputs are not in the right order" $ do
      areCorrectOrder "[[1],1]\n[[],2]" `shouldBe` [False]
    it "Left1: If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison" $ do
      areCorrectOrder "[[1],1]\n[1,2]" `shouldBe` [True]
    it "Left2: If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison" $ do
      areCorrectOrder "[[1],1,1]\n[1,2,0]" `shouldBe` [True]
    it "Left3: If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison" $ do
      areCorrectOrder "[[1],2]\n[1,1]" `shouldBe` [False]
    it "Left4: If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison" $ do
      areCorrectOrder "[[1],2,1]\n[1,1,2]" `shouldBe` [False]
    it "Right1: If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison" $ do
      areCorrectOrder "[1,1]\n[[1],2]" `shouldBe` [True]
    it "Right2: If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison" $ do
      areCorrectOrder "[1,1,1]\n[[1],2,0]" `shouldBe` [True]
    it "Right3: If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison" $ do
      areCorrectOrder "[1,2]\n[[1],1]" `shouldBe` [False]
    it "Right4: If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison" $ do
      areCorrectOrder "[1,2,1]\n[[1],1,2]" `shouldBe` [False]
      
  describe "part1" $ do
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      part1 contents `shouldBe` 13
      
  describe "part2" $ do
    it "works for markers" $ do
      part2 "[[3]]\n[[5]]" `shouldBe` 4
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      part2 contents `shouldBe` 140