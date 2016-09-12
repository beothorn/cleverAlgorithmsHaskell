{-# LANGUAGE NamedFieldPuns #-}

module Stochastic.AdaptativeRandomSearch
(
  adaptativeRandomSearch,
  takeStep
)
where
import System.Random
import Stochastic.Candidate
import Stochastic.Interval

data ARSParameters = ARSParams {
  initialFactor :: Double,
  smallStepFactor :: Double,
  largeStepFactor :: Double,
  tryBigStepEveryNTime :: Int,
  maxTriesWithoutImprovement :: Int
} 

adaptativeRandomSearch :: Int -> Problem -> ARSParameters -> [Double]
adaptativeRandomSearch
  iterationCount 
  problem@(Problem {interval=(Interval min max)})
  arsParams@(ARSParams {initialFactor}) = 
    searchBest
      iterationCount
      iterationsSinceLastImprovement
      newRandomCandidate
      stepSize
      problem
      arsParams
    where
      newRandomCandidate = randomCandidate problem
      stepSize = (max - min) * initialFactor
      iterationsSinceLastImprovement = 0

searchBest :: Int -> Int -> Candidate -> Double -> Problem -> ARSParameters -> [Double]
searchBest 0 _ (Candidate {solution = s}) _ _ _ = [0]
searchBest
  iterationCount 
  iterationsSinceLastImprovement
  best
  stepSize
  problem@(Problem {interval=(Interval min max)})
  arsParams@(ARSParams {tryBigStepEveryNTime, smallStepFactor, largeStepFactor}) = 
    searchBest
      nextIteration 
      0
      newRandomCandidate
      stepSize
      problem
      arsParams
    where
      nextIteration = iterationCount - 1
      iterationCountSinceLastImprovement =  iterationsSinceLastImprovement - 1 
      newRandomCandidate = randomCandidate problem
      newVariableStepSize = variableStepSize iterationCount tryBigStepEveryNTime stepSize smallStepFactor largeStepFactor  

variableStepSize :: Int -> Int -> Double -> Double -> Double -> Double 
variableStepSize iterationCount tryBigStepEveryNTime stepSize smallStepFactor largeStepFactor
  | iterationCount `mod` tryBigStepEveryNTime == 0 && iterationCount > 0 = stepSize * largeStepFactor
  | otherwise = stepSize * smallStepFactor

--takeStep (Problem (Interval (-5) 5) 2 (\ x -> sum x) (mkStdGen 42)) (Candidate [0,0] 0) 1
takeStep :: Problem -> Candidate -> Double -> Candidate
takeStep
  problem
  current
  stepSize = randomCandidate problem

