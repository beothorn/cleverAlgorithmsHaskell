module Stochastic.RandomSearch 
( 
  randomSearch  
)
where
import System.Random
import Stochastic.Candidate
import Stochastic.Interval

randomSearch :: Int -> Problem -> [Double]
randomSearch iterationCount problem = 
  searchBest 
    iterationCount 
    newRandomCandidate 
    problem 
  where
    newRandomCandidate = randomCandidate problem

searchBest :: Int -> Candidate -> Problem -> [Double]
searchBest 0 (Candidate {solution = s}) _ = s
searchBest iterationCount best problem = 
  searchBest 
    nextIteration
    tryToFindABestFit 
    problem 
  where
    nextIteration = (iterationCount - 1)
    newRandomCandidate = randomCandidate problem
    tryToFindABestFit = (minimum [best, newRandomCandidate])