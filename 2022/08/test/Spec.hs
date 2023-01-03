import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "part1" $ do
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      part1 contents `shouldBe` 21
    it "works for inputy 1" $ do
      contents <- readFile "input.txt"
      part1 contents `shouldBe` 1705
  describe "part2" $ do
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      part2 contents `shouldBe` 8