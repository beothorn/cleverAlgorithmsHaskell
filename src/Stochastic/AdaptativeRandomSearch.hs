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
  stepFactor
  smallStepFactor 
  largeStepFactor 
  noChangeMax
  costFunction 
  rndgen = [0]

searchBest :: Int -> Candidate -> Interval -> Int -> Double -> Double -> Double -> Double -> Double -> ([Double] -> Double) -> StdGen -> [Double]
searchBest
  iterationCount 
  best
  interval
  searchSpace 
  initialFactor 
  stepFactor
  smallStepFactor 
  largeStepFactor 
  noChangeMax
  costFunction 
  rndgen = [0]