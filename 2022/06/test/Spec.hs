import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "charactersToDiscard" $ do
    it "should handle example 1" $ do
      endOfFirstUniqueRun 4 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
    it "should handle example 2" $ do
      endOfFirstUniqueRun 4 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 6
    it "should handle example 3" $ do
      endOfFirstUniqueRun 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
    it "should handle example 4" $ do
      endOfFirstUniqueRun 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11

