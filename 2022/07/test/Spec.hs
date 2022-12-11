import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "totalSize" $ do
    it "should handle example 1" $ do
      contents <- readFile "example.txt"
      getDirectorySizes contents `shouldBe` [48381165,94853,584,24933642]
    it "should handle input" $ do
      contents <- readFile "input.txt"
      getDirectorySizes contents `shouldBe` [46552309,8827,980358,499575,378032,151219,226813,311263,409582,110296,110296,12700902,11282773,3867479,241085,1716764,1263886,287777,202578,202578,207736,245142,648077,153354,153354,299192,299192,291752,261438,6509648,516245,446056,4304391,465101,3168146,186579,469192,401684,1448626,1083298,746808,286129,173546,964377,208935,211651,803830,357320,188108,188108,188108,169212,67829,73872,73872,29001698,139505,22488591,2443664,525324,478969,672491,222900,3210462,285979,170398,1871102,1110906,808074,85166,217666,760196,82566,82566,68011,236818,5398364,367726,51018,51018,51018,3236774,1468108,1150248,113430,67634,401675,36205,1033579,85529,349046,47666,1305723,354235,136888,136888,89424,105537,2454786,233842,2039897,123190,1091724,291980,181047,8474158,1372337,279480,983672,79822,79822,707418,60317,171967,94404,560598,1805810,111452,207009,175736,237625,375947,206302,206302,322720,2588204,2047785,941352,643330,551089,92241,502499,333589,636560,193288,443272,276786,602560,358246,4363998,113574,95168,1667260,307371,173397,173397,410127,213821,196306,196306,367410,367410,212011,1158686,24056,222897,29139,605295,258300,3450942,148619,1885776,559440,41339,41339,273367,262982,320061,64024,159155,584138,304137,439769,391773,267972,585005]

  describe "totalSizeOfDirectoriesBelow" $ do
    it "should handle example 1" $ do
      contents <- readFile "example.txt"
      totalSizeOfDirectoriesBelow 100000 contents `shouldBe` 95437