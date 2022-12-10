import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
  describe "parseStackLine" $ do
    it "should handle example 1" $ do
      parseStackLine "[Z] [M] [P]" `shouldBe` "ZMP"
    it "should handle example 2" $ do
      parseStackLine "[M] [C]    " `shouldBe` "MC "
    it "should handle example 3" $ do
      parseStackLine "[D]        " `shouldBe` "D  "

  describe "parseStacks" $ do
    it "should handle example" $ do
      parseStacks [
        "[D]        ", 
        "[N] [C]    ", 
        "[Z] [M] [P]",
        " 1  2  3 "
        ] `shouldBe` ["DNZ", "CM", "P"]

  describe "parseFile" $ do
    it "should handle example" $ do
      contents <- readFile "example.txt"
      parseFile contents `shouldBe` (["NZ","DCM","P"],[(1,2,1),(3,1,3),(2,2,1),(1,1,2)])

      
  describe "applyInstruction" $ do
    it "should handle example" $ do
      contents <- readFile "example.txt"
      applyInstruction 1 (["NZ","DCM","P"],[(1,2,1),(3,1,3),(2,2,1),(1,1,2)]) `shouldBe`(["DNZ","CM","P"],[(3,1,3),(2,2,1),(1,1,2)])
      
  describe "applyInstructions" $ do
    it "should handle example" $ do
      contents <- readFile "example.txt"
      applyInstructions 1 (["NZ","DCM","P"],[(1,2,1),(3,1,3),(2,2,1),(1,1,2)]) `shouldBe` ["C","M","ZNDP"]
      
  describe "solvePartOne" $ do
    it "should handle example" $ do
      contents <- readFile "example.txt"
      solvePartOne contents `shouldBe` "CMZ"