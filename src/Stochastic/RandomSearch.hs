module Stochastic.RandomSearch where
import System.Random
import Stochastic.Candidate

result :: IO ()
result = do
  g <- newStdGen
  print $ show $ search (-5) 5 100 objective_function g

objective_function :: [Double] -> Double
objective_function nvect = sum $ map (**2) nvect

searchBest :: Double -> Double -> Int -> Candidate -> ([Double] -> Double) -> StdGen -> [Double]
searchBest _ _ 0 (Candidate {solution = s}) costFunction rndgen =  s
searchBest min max interationCount best costFunction rndgen = searchBest min max nextIteration (minimum [best, newRandomCandidate]) costFunction rndgen where
  nextIteration = (interationCount - 1)
  newRandomCandidate = randomCandidate min max costFunction rndgen

search :: Double -> Double -> Int -> ([Double] -> Double) -> StdGen -> [Double]
search min max interationCount costFunction rndgen = searchBest min max interationCount newRandomCandidate costFunction rndgen where
  newRandomCandidate = randomCandidate min max costFunction rndgen