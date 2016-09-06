module Stochastic.RandomSearch 
( 
  randomSearch  
)
where
import System.Random
import Stochastic.Candidate

randomSearch :: Double -> Double -> Int -> ([Double] -> Double) -> StdGen -> [Double]
randomSearch min max interationCount costFunction rndgen = searchBest min max interationCount newRandomCandidate costFunction rndgen where
  newRandomCandidate = randomCandidate min max costFunction rndgen

searchBest :: Double -> Double -> Int -> Candidate -> ([Double] -> Double) -> StdGen -> [Double]
searchBest _ _ 0 (Candidate {solution = s}) costFunction rndgen = s
searchBest min max interationCount best costFunction rndgen = searchBest min max nextIteration (minimum [best, newRandomCandidate]) costFunction rndgen where
  nextIteration = (interationCount - 1)
  newRandomCandidate = randomCandidate min max costFunction rndgen