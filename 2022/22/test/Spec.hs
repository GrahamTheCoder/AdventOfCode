import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "getPassword" $ do
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      getPassword contents `shouldBe` 6032
    it "works for input 1" $ do
      contents <- readFile "input.txt"
      getPassword contents `shouldBe` 191010

      