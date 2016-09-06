module Main where

import Stochastic.RandomSearch
import Stochastic.AdaptativeRandomSearch
import System.Random

objectiveFunction :: [Double] -> Double
objectiveFunction nvect = sum $ map (**2) nvect

main :: IO ()
main = do
  print "Random search" 
  g <- newStdGen
  let min = (-5)
      max = 5
      searchSpace = 2
      maxIterations = 100
    in print (randomSearch min max searchSpace maxIterations objectiveFunction g)
  print "Adaptative random search"
  let min = (-5)
      max = 5
      searchSpace = 2
      maxIterations = 1000
      initialFactor = 0.05
      smallStepFactor = 1.3
      largeStepFactor = 3.0
      stepFactor = 10
      noChangeMax = 30
    in print (adaptativeRandomSearch 
                min 
                max 
                searchSpace 
                maxIterations 
                initialFactor 
                stepFactor 
                smallStepFactor 
                largeStepFactor 
                noChangeMax 
                objectiveFunction 
                g
             )
    