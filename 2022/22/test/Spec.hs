import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "getPassword" $ do
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      getPassword contents `shouldBe` 6032