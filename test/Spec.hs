import System.Random
import Test.HUnit

newtype StaticGen = StaticGen [Int]
instance RandomGen StaticGen where
  next (StaticGen (x:xs)) = (x, StaticGen xs)
  next _ = error "No more random numbers!"
  split x = (x, x)           -- broken, but irrelevant
  genRange _ = (0, maxBound) -- choose your own

main :: IO ()
main = do
    runTestTT tests
    return ()

test1 = TestCase (assertEqual "for (foo 3)," 1 3)

tests = TestList [TestLabel "test1" test1]