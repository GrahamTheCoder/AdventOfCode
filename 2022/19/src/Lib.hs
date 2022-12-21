module Lib
    ( geodesProductionPlan, sumOfQualityLevels, productOfGeodesProduced
    ) where


import Control.Parallel.Strategies (parMap, rseq)
import Data.Char
import Data.Graph.AStar
import qualified Data.HashSet as HS
import Data.List.Split
import Data.Maybe
import Data.String.Utils
import Debug.Trace

data ResourceType = Ore | Clay | Obsidian | Geode deriving (Enum, Show, Eq, Read)

parseResource :: String -> ResourceType
parseResource (c:str) = read ((toUpper c):str)
parseResource r = error $ "Unknown resource " ++ show r

nonNegativeResources :: (Ord a1, Ord a2, Ord a3, Ord a4, Num a1, Num a2, Num a3, Num a4) => (a1, a2, a3, a4) -> Bool
nonNegativeResources (a,b,c,d) = a >=0 && b >= 0 && c >=0 && d >=0

-- Well known result, pair up elements 1+k and n-k and see they always sum to n+1
sumToN :: Integral a => a -> a
sumToN n = (n+1)*((n+1) `div` 2)

-- new robot created during this, and every other step, each then generating one geode per remaining step, so for 4 steps remaining, 0 + 1 + 2 + 3
maxGeneratedInSteps :: Integral a => a -> a
maxGeneratedInSteps n = sumToN (n-1)

addToPart :: Num d => ResourceType -> d -> (d, d, d, d) -> (d, d, d, d)
addToPart Ore amount (ore,cl,ob,ge) = (ore+amount,cl,ob,ge)
addToPart Clay amount (ore,cl,ob,ge) = (ore,cl+amount,ob,ge)
addToPart Obsidian amount (ore,cl,ob,ge) = (ore,cl,ob+amount,ge)
addToPart Geode amount (ore,cl,ob,ge) = (ore,cl,ob,ge+amount)

getPart :: ResourceType -> (a, a, a, a) -> a
getPart Ore (ore,_,_,_) = ore
getPart Clay (_,cl,_,_) = cl
getPart Obsidian (_,_,ob,_) = ob
getPart Geode (_,_,_,ge) = ge

-- tMap :: (a -> b) -> (a, a, a, a) -> [b]
-- tMap f (a,b,c,d) = map f [a,b,c,d]

-- listFrom :: (a, a, a, a) -> [a]
-- listFrom (a,b,c,d) = [a,b,c,d]

-- tupleFrom :: [a] ->  (a, a, a, a)
-- tupleFrom [a,b,c,d] = (a,b,c,d)

minus :: (Num a, Num b, Num c, Num d) => (a, b, c, d) -> (a, b, c, d) -> (a, b, c, d)
minus (a,b,c,d) (e,f,g,h) = (a-e, b-f, c-g, d-h)

plus :: (Num a, Num b, Num c, Num d) => (a, b, c, d) -> (a, b, c, d) -> (a, b, c, d)
plus (a,b,c,d) (e,f,g,h) = (a+e, b+f, c+g, d+h)

getPossibleStepTransition :: [String] -> ((Int,Int,Int,Int),ResourceType)
getPossibleStepTransition (_:robotType:_:_:allCosts) =
    let processPart [] x = x
        processPart ("and":costs) x = processPart costs x
        processPart (amount:partName:costs) x = processPart costs (addToPart (parseResource partName) (read amount) x)
        processPart lst _ = error $ "Can't process part " ++ show lst
        costTupl = processPart allCosts (0,0,0,0)
    in (costTupl, parseResource robotType)
getPossibleStepTransition x = error $ "Can't parse blueprint part " ++ show x

--default []
getTransitions :: String -> [((Int,Int,Int,Int),(Int,Int,Int,Int),Int) -> Maybe ((Int,Int,Int,Int),(Int,Int,Int,Int),Int)]
getTransitions blueprint = 
    let def ((ore, clay, obsidian, geodes), (oreBots, clayBots, obsidianBots, geodeBots), steps) = Just ((ore + oreBots, clay + clayBots, obsidian + obsidianBots, geodes + geodeBots), (oreBots, clayBots, obsidianBots, geodeBots), steps + 1)
        definition = head $ drop 1 $ splitOn ": " blueprint
        parts = fmap ((splitOn " ") . strip) $ splitOn "." definition
        filteredParts = filter (\outer -> outer /= []) $ fmap (filter (\inner -> inner /= "")) parts
        stepTransitions = fmap getPossibleStepTransition filteredParts
        maxRobotsNeeded = foldl (\(a,b,c,d) (e,f,g,h) -> (max a e, max b f, max c g, max d h)) (0,0,0,0) (map fst stepTransitions) 
        validTransition (cost, robotType) (resources, robots, n) = 
            let tempResources = resources `minus` cost
                isValid = nonNegativeResources tempResources
                isUseful = robotType == Geode || (getPart robotType robots) < (getPart robotType maxRobotsNeeded) -- perf optimization
            in if isValid && isUseful then
                Just (tempResources `plus` robots, addToPart robotType 1 robots, n+1)
                else Nothing
        validTransitions = fmap validTransition stepTransitions
    in (def:validTransitions)

underestimateRemainingDistance :: Integral a1 => a1 -> ((a2, b1, c1, d), (a3, b2, c2, a1), a1) -> a1
underestimateRemainingDistance maxSteps (_, (_, _, _, geodeBots), stepsSoFar) = 
    let remainingSteps = maxSteps - stepsSoFar
        definitelyGeneratedPerStep = geodeBots
        maxDistanceOfRemainingSteps = (maxGeneratedInSteps maxSteps) - (maxGeneratedInSteps stepsSoFar)
        definitelyGeneratedInRemaining = remainingSteps * definitelyGeneratedPerStep
        bestCaseAdditionalInRemaining = maxGeneratedInSteps remainingSteps
    in  maxDistanceOfRemainingSteps - definitelyGeneratedInRemaining - bestCaseAdditionalInRemaining

getNeighbours :: String -> ((Int, Int, Int, Int), (Int, Int, Int, Int), Int) -> HS.HashSet ((Int, Int, Int, Int), (Int, Int, Int, Int), Int)
getNeighbours blueprint s = 
    let allTransitions = getTransitions blueprint
    in HS.fromList $ fmap fromJust (filter isJust (map (\f -> f s) allTransitions))

-- Note: Constraint of creating one robot per minute is implicitly modelled by making each one an alternative
geodesProductionPlan :: Int -> String -> Maybe [((Int,Int,Int,Int),(Int,Int,Int,Int),Int)]
geodesProductionPlan maxSteps blueprint = 
    let getNeighbours' = getNeighbours blueprint
        -- Can never produce as many geodes per step as the step number since we can only produce one robot per step, so this is never negative
        distanceOfNode ((_,_,_,g1), _, _) ((_,_,_,g2), _, steps) = steps - 1 + g1 - g2
        underestimateRemainingDistance' = underestimateRemainingDistance maxSteps
        isEnd (_, _, t) = t == maxSteps
        start = ((0,0,0,0),(1,0,0,0),0)
    in aStar getNeighbours' distanceOfNode underestimateRemainingDistance' isEnd start
     
geodesProduced :: Int -> String -> Int
geodesProduced maxSteps blueprint =
    let ((_,_,_,g), _, _) = last $ fromJust (geodesProductionPlan maxSteps blueprint)
    in trace (show g) g

blueprintQuality :: Int -> String -> Int
blueprintQuality maxSteps blueprint = 
    let firstPart = head $ splitOn ": " blueprint
        idNum = read $ head $ drop 1 $ splitOn " " firstPart
        geodes = geodesProduced maxSteps blueprint
    in idNum * geodes

sumOfQualityLevels :: Int -> [String] -> Int
sumOfQualityLevels maxSteps blueprints = sum $ parMap rseq (blueprintQuality maxSteps) blueprints

productOfGeodesProduced :: Int -> [String] -> Int
productOfGeodesProduced maxSteps blueprints = product $ parMap rseq (geodesProduced maxSteps) blueprints