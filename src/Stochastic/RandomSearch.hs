module Stochastic.RandomSearch 
( 
  randomSearch  
)
where
import System.Random
import Stochastic.Candidate

randomSearch :: Double -> Double -> Int -> Int -> ([Double] -> Double) -> StdGen -> [Double]
randomSearch 
  min 
  max 
  searchSpace 
  iterationCount 
  costFunction 
  rndgen = 
    searchBest 
      iterationCount 
      newRandomCandidate 
      min 
      max 
      searchSpace 
      costFunction 
      rndgen where
        newRandomCandidate = randomCandidate min max searchSpace costFunction rndgen

searchBest :: Int -> Candidate -> Double -> Double -> Int -> ([Double] -> Double) -> StdGen -> [Double]
searchBest 0 (Candidate {solution = s}) _ _ _ _ _ = s
searchBest iterationCount best min max searchSpace costFunction rndgen = 
  searchBest 
    nextIteration
    tryToFindABestFit 
    min 
    max 
    searchSpace 
    costFunction 
    rndgen 
  where
    nextIteration = (iterationCount - 1)
    newRandomCandidate = randomCandidate min max searchSpace costFunction rndgen
    tryToFindABestFit = (minimum [best, newRandomCandidate])