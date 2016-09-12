module Stochastic.Candidate where
import System.Random
import Stochastic.Interval

data Problem = Problem {
  interval :: Interval, 
  searchSpace :: Int,
  costFunction :: ([Double] -> Double),
  randomGenerator :: StdGen
}  

data Candidate = Candidate {solution::[Double], cost::Double} deriving Show

instance Ord Candidate where
  (Candidate {cost = c1}) `compare` (Candidate { cost = c2}) = c1 `compare` c2

instance Eq Candidate where
  (Candidate {cost = c1}) == (Candidate { cost = c2}) = c1 == c2

createCandidate ::  [Double] -> ([Double] -> Double) -> Candidate
createCandidate vector costFunction = Candidate {solution=vector, cost= costFunction vector}

randomCandidate :: Problem -> Candidate
randomCandidate (Problem interval searchSpace costFunction g) = createCandidate  (take searchSpace $ values interval g ) costFunction

values :: Interval -> StdGen -> [Double]
values (Interval min max) rndgen = map fst $ scanl (\(r, gen) _ -> random gen) (randomR (min, max) rndgen) $ repeat () 