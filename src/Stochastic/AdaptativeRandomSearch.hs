module Stochastic.AdaptativeRandomSearch
(
  adaptativeRandomSearch
)
where
import System.Random
import Stochastic.Candidate
import Stochastic.Interval

adaptativeRandomSearch :: Interval -> Int -> Int -> Double -> Double -> Double -> Double -> Double -> ([Double] -> Double) -> StdGen -> [Double]
adaptativeRandomSearch 
  interval
  searchSpace 
  iterationCount 
  initialFactor 
  tryBigStepEveryNTime
  smallStepFactor 
  largeStepFactor 
  noChangeMax
  costFunction 
  rndgen = [0]

searchBest :: Int -> Candidate -> Interval -> Int -> Double -> Double -> Double -> Double -> Double -> ([Double] -> Double) -> StdGen -> [Double]
searchBest 0 (Candidate {solution = s}) _ _ _ _ _ _ _ _ _ = [0]
searchBest
  iterationCount 
  best
  interval@(Interval max min)
  searchSpace 
  initialFactor 
  tryBigStepEveryNTime
  smallStepFactor 
  largeStepFactor 
  noChangeMax
  costFunction 
  rndgen = 
    searchBest
      (iterationCount - 1) 
      newRandomCandidate
      interval
      searchSpace 
      initialFactor 
      tryBigStepEveryNTime
      smallStepFactor 
      largeStepFactor 
      noChangeMax
      costFunction 
      rndgen
    where
      newRandomCandidate = randomCandidate interval searchSpace costFunction rndgen
      stepSize = (max - min) * initialFactor

variableStepSize :: Int -> Int -> Double -> Double -> Double -> Double 
variableStepSize iterationCount tryBigStepEveryNTime stepSize smallStepFactor largeStepFactor
  | iterationCount `mod` tryBigStepEveryNTime == 0 && iterationCount > 0 = stepSize * largeStepFactor
  | otherwise = stepSize * smallStepFactor
