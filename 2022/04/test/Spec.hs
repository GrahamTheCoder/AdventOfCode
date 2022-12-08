import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
  describe "parseLine" $ do
    it "works for 2-4,6-8" $ do
      parseLine "2-4,6-8" `shouldBe` [(2,4),(6,8)]

    it "parses lines of non-negative start and length" $ do
      property $ \a b c d -> let
        a' = abs a
        b' = abs b
        c' = abs c
        d' = abs d
        in parseLine (show a' ++ "-" ++ show (a'+b') ++ "," ++ show c' ++ "-" ++ show (c'+d')) == [(a',a'+b'),(c',c'+d')]