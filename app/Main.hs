module Main where

import Stochastic.Candidate
import Stochastic.RandomSearch
import Stochastic.AdaptativeRandomSearch
import Stochastic.Interval
import System.Random

objectiveFunction :: [Double] -> Double
objectiveFunction nvect = sum $ map (**2) nvect

main :: IO ()
main = do
  print "Random search" 
  g <- newStdGen
  let interval = Interval (-5) 5
      searchSpace = 2
      maxIterations = 100
      problem = Problem interval searchSpace objectiveFunction g
    in print $ randomSearch maxIterations problem
  print "Adaptative random search"
--   let interval = Interval (-5) 5
--       searchSpace = 2
--       maxIterations = 1000
--       initialFactor = 0.05
--       smallStepFactor = 1.3
--       largeStepFactor = 3.0
--       tryBigStepEveryNTime = 10
--       noChangeMax = 30
--     in print (adaptativeRandomSearch 
--                 interval
--                 searchSpace 
--                 maxIterations 
--                 initialFactor 
--                 tryBigStepEveryNTime 
--                 smallStepFactor 
--                 largeStepFactor 
--                 noChangeMax 
--                 objectiveFunction 
--                 g
--              )
    