module Stochastic.RandomSearch 
( 
  randomSearch  
)
where
import System.Random
import Stochastic.Candidate
import Stochastic.Interval

randomSearch :: Interval -> Int -> Int -> ([Double] -> Double) -> StdGen -> [Double]
randomSearch 
  interval
  searchSpace 
  iterationCount 
  costFunction 
  rndgen = 
    searchBest 
      iterationCount 
      newRandomCandidate 
      interval
      searchSpace 
      costFunction 
      rndgen 
    where
      newRandomCandidate = randomCandidate interval searchSpace costFunction rndgen

searchBest :: Int -> Candidate -> Interval -> Int -> ([Double] -> Double) -> StdGen -> [Double]
searchBest 0 (Candidate {solution = s}) _ _ _ _ = s
searchBest iterationCount best interval searchSpace costFunction rndgen = 
  searchBest 
    nextIteration
    tryToFindABestFit 
    interval
    searchSpace 
    costFunction 
    rndgen 
  where
    nextIteration = (iterationCount - 1)
    newRandomCandidate = randomCandidate interval searchSpace costFunction rndgen
    tryToFindABestFit = (minimum [best, newRandomCandidate])