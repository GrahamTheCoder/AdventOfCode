import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "getRootValue" $ do
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      getRootValue (lines contents) `shouldBe` 152