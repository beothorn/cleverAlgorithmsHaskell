module Main where

import Stochastic.RandomSearch
import System.Random

objective_function :: [Double] -> Double
objective_function nvect = sum $ map (**2) nvect

main :: IO ()
main = do
  print "Random search" 
  g <- newStdGen
  print (randomSearch (-5) 5 100 objective_function g)
