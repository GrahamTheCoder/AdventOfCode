import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
  describe "parseLine" $ do
    it "works for 2-4,6-8" $ do
      parseLine "2-4,6-8" `shouldBe` ((2, 4), (6, 8))

    it "parses lines of non-negative start and length" $ do
      property $ \a b c d -> let
        a' = abs a
        b' = abs b
        c' = abs c
        d' = abs d
        in parseLine (show a' ++ "-" ++ show (a'+b') ++ "," ++ show c' ++ "-" ++ show (c'+d')) == ((a', (a' + b')), (c', (c'+d')))
        
  describe "fullOverlap" $ do
    it "is false for 2-4,6-8" $ do
      fullOverlap (2, 4) (6, 8) `shouldBe` False

    it "is false when first range starts and ends later" $ do
      property $ \a' b' -> 
        let a = min a' b'; b = max a' (b' :: Int)
        in fullOverlap (a+1, b+1) (a, b) `shouldBe` False

    it "is false when second range starts and ends later" $ do
      property $ \a' b' -> 
        let a = min a' b'; b = max a' (b' :: Int)
        in fullOverlap (a, b) (a+1, b+1) `shouldBe` False

    it "is true when first range starts later but ends the same" $ do
      property $ \a' b' -> 
        let a = min a' b'; b = max a' (b' :: Int)
        in fullOverlap (a+1, b) (a, b) `shouldBe` True

    it "is true when second range starts later but ends the same" $ do
      property $ \a' b' -> 
        let a = min a' b'; b = max a' (b' :: Int)
        in fullOverlap (a, b) (a+1, b) `shouldBe` True
    
    it "is true when first range starts earlier and ends later" $ do
      property $ \a' b' -> 
        let a = min a' b'; b = max a' (b' :: Int)
        in fullOverlap (a-1, b+1) (a, b) `shouldBe` True
    
    it "is true when second range starts earlier and ends later" $ do
      property $ \a' b' -> 
        let a = min a' b'; b = max a' (b' :: Int)
        in fullOverlap (a, b) (a-1, b+1) `shouldBe` True