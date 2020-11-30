module QCStn (tests) where

import qualified Test.Framework as Framework
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HUnit
  ( assertEqual
  , Test(..)
  )

import HaskellTemporalLib.Tools
  ( constrain
  , SimpleTemporalNetwork(..)
  , ModifiableNet(..)
  , STNMap(..)
  )

tests :: [Framework.Test]
tests = hUnitTestToTests $ TestList
  [ tSTNMap1
  , tSTNMapModification1
  ]

-- -----------------------------------------------------------------------------
-- Fixtures
-- -----------------------------------------------------------------------------
stnMap1 :: STNMap Char Double
stnMap1 = fromList 'z'
            $  constrain 'z' 'a' 10.0    10.0
            ++ constrain 'a' 'b' 200.0   200.0
            ++ constrain 'b' 'c' 300.0   3000.0
            ++ constrain 'c' 'd' 40000.0 40000.0
-- -----------------------------------------------------------------------------
-- Test Cases
-- -----------------------------------------------------------------------------

tSTNMap1 :: Test
tSTNMap1 = TestLabel "Construction1"
    (TestCase
        (assertEqual "Num events should be 5"
          (length $ events stnMap1) 5
        )
    )

tSTNMapModification1 :: Test
tSTNMapModification1 = TestLabel "Modification1"
    (TestCase
        (assertEqual "Num events should be 5"
          (cnst 'z' 'a' $ setCnst 'z' 'a' 5.0 stnMap1) (Just 5.0)
        )
    )
