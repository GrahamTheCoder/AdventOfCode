import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "getValue" $ do
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      getValue (lines contents) "root" `shouldBe` 152
    it "works for input 1 root" $ do
      contents <- readFile "input.txt"
      getValue (lines contents) "root" `shouldBe` 299983725663456
    it "works for input 1 lttc" $ do
      contents <- readFile "input.txt"
      getValue (lines contents) "lttc" `shouldBe` 210322230761488
    it "works for input 1 pfjc" $ do
      contents <- readFile "input.txt"
      getValue (lines contents) "pfjc" `shouldBe` 89661494901968
      
  describe "partTwo" $ do
    it "works for example 1" $ do
      contents <- readFile "example.txt"
      partTwo (lines contents) "root" `shouldBe` 301
    it "works for input 1 root" $ do
      contents <- readFile "input.txt"
      partTwo (lines contents) "root" `shouldBe` 3093175982595
    