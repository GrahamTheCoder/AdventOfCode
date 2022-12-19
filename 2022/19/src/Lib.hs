module Lib
    ( geodesProductionPlan, sumOfQualityLevels
    ) where


import qualified Data.HashSet as HS
import Data.Graph.AStar
import Data.List.Split
import Data.Maybe
import Data.String.Utils

nonNegativeResources (a,b,c,d) = a >=0 && b >= 0 && c >=0 && d >=0

-- Well known result, pair up elements 1+k and n-k and see they always sum to n+1
sumToN n = (n+1)*((n+1) `div` 2)

-- new robot created during this, and every other step, each then generating one geode per remaining step, so for 4 steps remaining, 0 + 1 + 2 + 3
maxGeneratedInSteps n = sumToN (n-1)

addToPart "ore" amount (or,cl,ob,ge) = (or+amount,cl,ob,ge)
addToPart "clay" amount (or,cl,ob,ge) = (or,cl+amount,ob,ge)
addToPart "obsidian" amount (or,cl,ob,ge) = (or,cl,ob+amount,ge)
addToPart "geode" amount (or,cl,ob,ge) = (or,cl,ob,ge+amount)
addToPart "geodes" amount (or,cl,ob,ge) = (or,cl,ob,ge+amount)

minus (a,b,c,d) (e,f,g,h) = (a-e, b-f, c-g, d-h)
plus (a,b,c,d) (e,f,g,h) = (a+e, b+f, c+g, d+h)

getPossibleStepTransition :: [String] -> ((Int,Int,Int,Int),(Int,Int,Int,Int) -> (Int,Int,Int,Int))
getPossibleStepTransition (_:robotType:_:_:allCosts) =
    let processPart [] x = x
        processPart ("and":costs) x = processPart costs x
        processPart (amount:partName:costs) x = processPart costs (addToPart partName (read amount) x)
        costTupl = processPart allCosts (0,0,0,0)
    in (costTupl, addToPart robotType 1)
getPossibleStepTransition x = error $ show x

--default []
getTransitions :: String -> [((Int,Int,Int,Int),(Int,Int,Int,Int),Int) -> Maybe ((Int,Int,Int,Int),(Int,Int,Int,Int),Int)]
getTransitions blueprint = 
    let def ((ore, clay, obsidian, geodes), (oreBots, clayBots, obsidianBots, geodeBots), steps) = Just ((ore + oreBots, clay + clayBots, obsidian + obsidianBots, geodes + geodeBots), (oreBots, clayBots, obsidianBots, geodeBots), steps + 1)
        definition = head $ drop 1 $ splitOn ": " blueprint
        parts = fmap ((splitOn " ") . strip) $ splitOn "." definition
        filteredParts = filter (\outer -> outer /= []) $ fmap (filter (\inner -> inner /= "")) parts
        stepTransitions = fmap getPossibleStepTransition filteredParts
        maxRobotsNeeded = maximum $ map fst stepTransitions
        validTransition (cost, addRobots) (resources, robots, n) = 
            let tempResources = resources `minus` cost
                endRobots = ((addRobots robots) :: (Int,Int,Int,Int))
                isValid = nonNegativeResources tempResources
                isUseful = endRobots <= maxRobotsNeeded -- perf optimization
            in if isValid && isUseful then
                Just (tempResources `plus` robots, endRobots, n+1)
                else Nothing
        validTransitions = fmap validTransition stepTransitions
    in (def:validTransitions)

underestimateRemainingDistance :: Integral a1 => a1 -> ((a2, b1, c1, d), (a3, b2, c2, a1), a1) -> a1
underestimateRemainingDistance maxSteps ((_, _, _, geodes), (_, _, _, geodeBots), stepsSoFar) = 
    let remainingSteps = maxSteps - stepsSoFar
        definitelyGeneratedPerStep = geodeBots
        maxDistanceOfRemainingSteps = (maxGeneratedInSteps maxSteps) - (maxGeneratedInSteps stepsSoFar)
        definitelyGeneratedInRemaining = remainingSteps * definitelyGeneratedPerStep
        bestCaseAdditionalInRemaining = maxGeneratedInSteps remainingSteps
    in  maxDistanceOfRemainingSteps - definitelyGeneratedInRemaining - bestCaseAdditionalInRemaining

getNeighbours blueprint s = 
    let allTransitions = getTransitions blueprint
    in HS.fromList $ fmap (\(Just x) -> x)  (filter isJust (map (\f -> f s) allTransitions))

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
    in g

blueprintQuality :: Int -> String -> Int
blueprintQuality maxSteps blueprint = 
    let firstPart = head $ splitOn ": " blueprint
        (_:idStr:rest) = splitOn " " firstPart
        id = read idStr
        geodes = geodesProduced maxSteps blueprint
    in id * geodes

sumOfQualityLevels :: Int -> [String] -> Int
sumOfQualityLevels maxSteps blueprints = sum $ fmap (blueprintQuality maxSteps) blueprints