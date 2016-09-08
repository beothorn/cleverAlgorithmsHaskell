module Stochastic.Candidate where
import System.Random
import Stochastic.Interval

data Candidate = Candidate {solution::[Double], cost::Double} 

instance Ord Candidate where
  (Candidate {cost = c1}) `compare` (Candidate { cost = c2}) = c1 `compare` c2

instance Eq Candidate where
  (Candidate {cost = c1}) == (Candidate { cost = c2}) = c1 == c2

createCandidate ::  [Double] -> ([Double] -> Double) -> Candidate
createCandidate vector costFunction = Candidate {solution=vector, cost= costFunction vector}

randomCandidate :: Interval -> Int -> ([Double] -> Double) -> StdGen -> Candidate
randomCandidate interval searchSpace costFunction g = createCandidate  (take searchSpace $ values interval g ) costFunction

values :: Interval -> StdGen -> [Double]
values (Interval min max) rndgen = map fst $ scanl (\(r, gen) _ -> random gen) (randomR (min, max) rndgen) $ repeat () 