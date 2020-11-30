module QCGraph where

import Prelude hiding (lookup)

import qualified Test.Framework as Framework
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HUnit
  ( assertEqual
  , assertBool
  , Test(..)
  )

import Data.Maybe
import Data.Map.Strict (Map, fromList, lookup, insert)

import HaskellTemporalLib.Tools (ifpc, floydWarshall)

-- Tests to export.

tests :: [Framework.Test]
tests = hUnitTestToTests $ TestList
  [ tIFPC1
  , tIFPC2
  , tIFPC3
  , tIFPC4
  , tFloydWarshall1
  ]

-- Test definitions and constants.

verts :: [Int]
verts = [1,2,3]

weights :: Map (Int, Int) Double
weights = fromList [ ((1, 2), 14.0)
                   , ((3, 1), 3.0)
                   , ((3, 2), 17.0)
                   , ((1, 3), 7.0)
                   , ((2, 3), 3.0)
                   , ((2, 1), 6.0)

                   , ((1, 1), 0.0)
                   , ((2, 2), 0.0)
                   , ((3, 3), 0.0)
                   ]


tIFPC1 :: Test
tIFPC1 = TestLabel "Identity"
  (TestCase
    (assertEqual "Change detected when none should exist"
      (fromJust (ifpc ((1, 2), 14.0) verts weights)) weights
    )
  )


tIFPC2 :: Test
tIFPC2 = TestLabel "Relaxed"
  (TestCase
    (assertEqual "Change detected when none should exist"
      (fromJust (ifpc ((1, 2), 17.0) verts weights)) weights
    )
  )


tIFPC3 :: Test
tIFPC3 = TestLabel "Tighten"
  (TestCase
    (assertBool "Change detected when some should exist"
      (fromJust (ifpc ((1, 2), 10.0) verts weights) /= weights)
    )
  )


tIFPC4 :: Test
tIFPC4 = TestLabel "APSP Comparison 1"
  (TestCase
    (assertEqual "`ifpc` did not produce a minimal graph"
      (floydWarshall verts (`lookup` insert (1, 2) 10.0 weights))
      (fromJust (ifpc ((1, 2), 10.0) verts weights))
    )
  )


tFloydWarshall1 :: Test
tFloydWarshall1 = TestLabel "Repeat minimisation"
  (TestCase $
    let
      minimise w = floydWarshall verts (`lookup` w)
    in
      assertEqual
        ("floydWarshall should not change"
          ++ " already minimised graphs")
        (minimise weights)
        (iterate minimise weights !! 2)
  )

