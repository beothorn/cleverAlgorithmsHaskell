module Stochastic.Candidate where
import System.Random

data Candidate = Candidate {solution::[Double], cost::Double} 

instance Ord Candidate where
  (Candidate {cost = c1}) `compare` (Candidate { cost = c2}) = c1 `compare` c2

instance Eq Candidate where
  (Candidate {cost = c1}) == (Candidate { cost = c2}) = c1 == c2

createCandidate ::  [Double] -> ([Double] -> Double) -> Candidate
createCandidate vector costFunction = Candidate {solution=vector, cost= costFunction vector}

randomCandidate :: Double -> Double -> ([Double] -> Double) -> StdGen -> Candidate
randomCandidate min max costFunction g = createCandidate  (take 2 $ values min max g ) costFunction

values :: Double -> Double -> StdGen -> [Double]
values min max rndgen = map fst $ scanl (\(r, gen) _ -> random gen) (randomR (min, max) rndgen) $ repeat () 