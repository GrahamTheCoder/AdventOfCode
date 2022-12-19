import Data.Maybe
import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "geodesProductionPlan" $ do
    it "characterize currently produced plan for example 1" $ do
      contents <- readFile "example.txt"
      plan <- pure $ geodesProductionPlan 24 (head (lines contents))
      ((_,_,_,geodes), _, n) <- pure $ last (fromJust plan)
      putStrLn $ show plan
      (geodes, n) `shouldBe` (9, 24)
      -- If this fails but still ends with 9 geodes, that's not necessarily a problem, just a flag that something has changed
      plan `shouldBe` Just [((1,0,0,0),(1,0,0,0),1),((2,0,0,0),(1,0,0,0),2),((3,0,0,0),(1,0,0,0),3),((2,0,0,0),(1,1,0,0),4),((1,1,0,0),(1,2,0,0),5),((2,3,0,0),(1,2,0,0),6),((1,5,0,0),(1,3,0,0),7),((2,8,0,0),(1,3,0,0),8),((3,11,0,0),(1,3,0,0),9),((4,14,0,0),(1,3,0,0),10),((2,3,0,0),(1,3,1,0),11),((1,6,1,0),(1,4,1,0),12),((2,10,2,0),(1,4,1,0),13),((3,14,3,0),(1,4,1,0),14),((1,4,4,0),(1,4,2,0),15),((2,8,6,0),(1,4,2,0),16),((3,12,8,0),(1,4,2,0),17),((2,16,3,0),(1,4,2,1),18),((3,20,5,1),(1,4,2,1),19),((4,24,7,2),(1,4,2,1),20),((3,28,2,3),(1,4,2,2),21),((4,32,4,5),(1,4,2,2),22),((1,36,6,7),(2,4,2,2),23),((3,40,8,9),(2,4,2,2),24)]
    it "characterize currently produced plan for example 2" $ do
      contents <- readFile "example.txt"
      plan <- pure $ geodesProductionPlan 32 (head (lines contents))
      putStrLn $ show plan
      ((_,_,_,geodes), _, n) <- pure $ last (fromJust plan)
      (geodes, n) `shouldBe` (56, 32)
      -- If this fails but still ends with 56 geodes, that's not necessarily a problem, just a flag that something has changed
      plan `shouldBe` Just [((1,0,0,0),(1,0,0,0),1),((2,0,0,0),(1,0,0,0),2),((3,0,0,0),(1,0,0,0),3),((4,0,0,0),(1,0,0,0),4),((1,0,0,0),(2,0,0,0),5),((3,0,0,0),(2,0,0,0),6),((3,0,0,0),(2,1,0,0),7),((3,1,0,0),(2,2,0,0),8),((3,3,0,0),(2,3,0,0),9),((3,6,0,0),(2,4,0,0),10),((3,10,0,0),(2,5,0,0),11),((3,15,0,0),(2,6,0,0),12),((3,21,0,0),(2,7,0,0),13),((2,14,0,0),(2,7,1,0),14),((4,21,1,0),(2,7,1,0),15),((3,14,2,0),(2,7,2,0),16),((2,7,4,0),(2,7,3,0),17),((4,14,7,0),(2,7,3,0),18),((3,7,10,0),(2,7,4,0),19),((3,14,7,0),(2,7,4,1),20),((2,7,11,1),(2,7,5,1),21),((2,14,9,2),(2,7,5,2),22),((2,21,7,4),(2,7,5,3),23),((2,28,5,7),(2,7,5,4),24),((2,35,10,11),(2,8,5,4),25),((2,43,8,15),(2,8,5,5),26),((2,51,6,20),(2,8,5,6),27),((2,59,11,26),(2,9,5,6),28),((2,68,9,32),(2,9,5,7),29),((2,77,7,39),(2,9,5,8),30),((2,86,5,47),(2,9,5,9),31),((2,95,10,56),(2,10,5,9),32)]

  describe "sumOfQualityLevels" $ do
    it "should calculate quality for example 1" $ do
      contents <- readFile "example.txt"
      sumOfQualityLevels 24 (lines contents) `shouldBe` 33