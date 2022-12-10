import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
  describe "0" $ do
    it "0" $ do
      0 `shouldBe` 0