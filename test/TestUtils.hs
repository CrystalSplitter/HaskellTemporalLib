module TestUtils where

import           Test.HUnit                    as HUnit
import           Text.Printf

defaultTolerance :: (Floating a) => a
defaultTolerance = 10.0 ** (-6.0)

realInTol :: (RealFloat a) => a -> a -> Bool
realInTol x y | isInfinite x && isInfinite y = True
              | otherwise                    = abs (x - y) <= defaultTolerance

assertApprox :: (RealFloat a, PrintfArg a) => String -> a -> a -> IO ()
assertApprox s x y = HUnit.assertBool
  (printf "%s\nexpected: %f+/-%v\n but got: %f"
          s
          x
          (defaultTolerance :: Double)
          y :: String
  )
  (realInTol x y)
