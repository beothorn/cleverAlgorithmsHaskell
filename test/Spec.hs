import System.Random
import Test.HUnit
import Stochastic.AdaptativeRandomSearchTest

newtype StaticGen = StaticGen [Int]
instance RandomGen StaticGen where
  next (StaticGen (x:xs)) = (x, StaticGen xs)
  next _ = error "No more random numbers!"
  split x = (x, x)           -- broken, but irrelevant
  genRange _ = (0, 100) -- choose your own

main :: IO ()
main = do
    let gen = StaticGen [5,10,15,10,15,15,15,15]
    let ns = randoms gen :: [Int]
    print $ take 1 $ ns
    runTestTT tests
    return ()

test1 = TestCase (assertEqual "gen a random number" 1 2)

tests = TestList [TestLabel "test1" test1]