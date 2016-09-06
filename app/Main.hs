module Main where

import Stochastic.RandomSearch
import System.Random

objectiveFunction :: [Double] -> Double
objectiveFunction nvect = sum $ map (**2) nvect

main :: IO ()
main = do
  print "Random search" 
  g <- newStdGen
  print (randomSearch min max searchSpace maxIterations objectiveFunction g) where
    min = (-5)
    max = 5
    searchSpace = 2
    maxIterations = 100