module Stochastic.AdaptativeRandomSearch
(
  adaptativeRandomSearch
)
where
import System.Random

adaptativeRandomSearch :: Double -> Double -> Int -> Int -> Double -> Double -> Double -> Double -> Double -> ([Double] -> Double) -> StdGen -> [Double]
adaptativeRandomSearch 
  min 
  max 
  searchSpace 
  iterationCount 
  initialFactor 
  stepFactor
  smallStepFactor 
  largeStepFactor 
  noChangeMax
  costFunction 
  rndgen = [0]

searchBest :: Int -> Candidate -> Double -> Double -> Int -> Int -> Double -> Double -> Double -> Double -> Double -> ([Double] -> Double) -> StdGen -> [Double]
  iterationCount 
  best
  min 
  max 
  searchSpace 
  iterationCount 
  initialFactor 
  stepFactor
  smallStepFactor 
  largeStepFactor 
  noChangeMax
  costFunction 
  rndgen = [0]