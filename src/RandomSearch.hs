module RandomSearch where
import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, RandomGen, mkStdGen, newStdGen, randomR, random)
import Control.Applicative ((<$>))

data Candidate = Candidate {vector::[Double], cost::Double} 

instance Ord Candidate where
  (Candidate {cost = c1}) `compare` (Candidate { cost = c2}) = c1 `compare` c2

instance Eq Candidate where
  (Candidate {cost = c1}) == (Candidate { cost = c2}) = c1 == c2

createCandidate ::  [Double] -> Candidate
createCandidate vector = Candidate {vector=vector, cost= objective_function vector}

randomCandidate :: Double -> Double -> StdGen -> Candidate
randomCandidate min max g = createCandidate $ take 2 $ values min max g

objective_function :: [Double] -> Double
objective_function nvect = sum $ map (**2) nvect

values :: Double -> Double -> StdGen -> [Double]
values min max rndgen = map fst $ scanl (\(r, gen) _ -> random gen) (randomR (min, max) rndgen) $ repeat ()

searchBest :: Double -> Double -> Int -> Candidate -> StdGen -> [Double]
searchBest _ _ 0 (Candidate {vector = v}) rndgen =  v
searchBest min max interationCount best rndgen = searchBest min max (interationCount - 1) (minimum [best, randomCandidate min max rndgen]) rndgen

search :: Double -> Double -> Int -> StdGen -> [Double]
search min max interationCount rndgen = searchBest min max interationCount (randomCandidate min max rndgen) rndgen

result :: IO ()
result = do
  g <- newStdGen
  print $ show $ search (-5) 5 100 g