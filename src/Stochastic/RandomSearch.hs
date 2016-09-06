module Stochastic.RandomSearch 
( 
  randomSearch  
)
where
import System.Random
import Stochastic.Candidate

randomSearch :: Double -> Double -> Int -> Int -> ([Double] -> Double) -> StdGen -> [Double]
randomSearch min max searchSpace interationCount costFunction rndgen = searchBest min max searchSpace interationCount newRandomCandidate costFunction rndgen where
  newRandomCandidate = randomCandidate min max searchSpace costFunction rndgen

searchBest :: Double -> Double -> Int -> Int -> Candidate -> ([Double] -> Double) -> StdGen -> [Double]
searchBest _ _ _ 0 (Candidate {solution = s}) costFunction rndgen = s
searchBest min max searchSpace interationCount best costFunction rndgen = 
  searchBest 
    min 
    max 
    searchSpace 
    nextIteration 
    tryToFindABestFit 
    costFunction 
    rndgen 
  where
    nextIteration = (interationCount - 1)
    newRandomCandidate = randomCandidate min max searchSpace costFunction rndgen
    tryToFindABestFit = (minimum [best, newRandomCandidate])