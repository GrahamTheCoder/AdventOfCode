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