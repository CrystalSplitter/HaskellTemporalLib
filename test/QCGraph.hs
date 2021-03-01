module QCGraph where

import           Prelude                 hiding ( lookup )

import qualified Test.Framework                as Framework
import           Test.Framework.Providers.HUnit ( hUnitTestToTests )
import           Test.HUnit                    as HUnit
                                                ( Test(..)
                                                , assertBool
                                                , assertEqual
                                                )

import           Data.Map.Strict                ( Map
                                                , fromList
                                                , insert
                                                , lookup
                                                )
import           Data.Maybe

import           HaskellTemporalLib.Internal.Graph.Graph
                                               as G
                                                ( floydWarshall
                                                )
import           HaskellTemporalLib.Internal.Graph.Mut.VectorGraph
                                               as VG
                                                ( floydWarshall, ifpc )

-- Tests to export.

tests :: [Framework.Test]
tests = hUnitTestToTests $ TestList
  [ tIFPC1
  , tIFPC2
  , tIFPC3
  , tIFPC4
  , tFloydWarshall1
  , tVectorFloydWarshall1
  , tVectorFloydWarshall2
  ]

-- Test definitions and constants.

verts :: [Int]
verts = [1, 2, 3]

weightsTC1 :: Map (Int, Int) Double
weightsTC1 = fromList
  [ ((1, 2), 14.0)
  , ((3, 1), 3.0)
  , ((3, 2), 17.0)
  , ((1, 3), 7.0)
  , ((2, 3), 3.0)
  , ((2, 1), 6.0)
  , ((1, 1), 0.0)
  , ((2, 2), 0.0)
  , ((3, 3), 0.0)
  ]

weightsTC2 :: Map (Int, Int) Double
weightsTC2 = fromList
  [ ((1, 1), 0.0)
  , ((1, 2), 3.0)
  , ((1, 4), 5.0)
  , ((2, 1), 2.0)
  , ((2, 2), 0.0)
  , ((3, 3), 0.0)
  , ((4, 3), 2.0)
  , ((2, 4), 4.0)
  , ((3, 2), 1.0)
  , ((4, 4), 0.0)
  ]

weightsTC2APSP :: Map (Int, Int) Double
weightsTC2APSP = fromList
  [ ((1, 1), 0.0)
  , ((1, 2), 3.0)
  , ((1, 3), 7.0)
  , ((1, 4), 5.0)
  , ((2, 1), 2.0)
  , ((2, 2), 0.0)
  , ((2, 3), 6.0)
  , ((2, 4), 4.0)
  , ((3, 1), 3.0)
  , ((3, 2), 1.0)
  , ((3, 3), 0.0)
  , ((3, 4), 5.0)
  , ((4, 1), 5.0)
  , ((4, 2), 3.0)
  , ((4, 3), 2.0)
  , ((4, 4), 0.0)
  ]


tIFPC1 :: Test
tIFPC1 = TestLabel
  "Identity"
  (TestCase
    (assertEqual "Change detected when none should exist"
                 (fromJust (VG.ifpc ((1, 2), 14.0) verts weightsTC1))
                 weightsTC1
    )
  )


tIFPC2 :: Test
tIFPC2 = TestLabel
  "Relaxed"
  (TestCase
    (assertEqual "Change detected when none should exist"
                 (fromJust (VG.ifpc ((1, 2), 17.0) verts weightsTC1))
                 weightsTC1
    )
  )


tIFPC3 :: Test
tIFPC3 = TestLabel
  "Tighten"
  (TestCase
    (assertBool
      "Change detected when some should exist"
      (fromJust (VG.ifpc ((1, 2), 10.0) verts weightsTC1) /= weightsTC1)
    )
  )


tIFPC4 :: Test
tIFPC4 = TestLabel
  "APSP Comparison 1"
  (TestCase
    (assertEqual
      "`ifpc` did not produce a minimal graph"
      (G.floydWarshall verts (`lookup` insert (1, 2) 10.0 weightsTC1))
      (fromJust (VG.ifpc ((1, 2), 10.0) verts weightsTC1))
    )
  )


tFloydWarshall1 :: Test
tFloydWarshall1 = TestLabel
  "Repeat minimisation (Map)"
  ( TestCase
  $ let minimise w = G.floydWarshall verts (`lookup` w)
    in  assertEqual
          ("floydWarshall should not change" ++ " already minimised graphs")
          (minimise weightsTC1)
          (iterate minimise weightsTC1 !! 2)
  )

tVectorFloydWarshall1 :: Test
tVectorFloydWarshall1 = TestLabel
  "Repeat minimisation (Vector)"
  ( TestCase
  $ let minimise w = VG.floydWarshall verts (`lookup` w)
    in  assertEqual
          ("floydWarshall should not change" ++ " already minimised graphs")
          (minimise weightsTC1)
          (iterate minimise weightsTC1 !! 2)
  )

tVectorFloydWarshall2 :: Test
tVectorFloydWarshall2 = TestLabel
  "Repeat minimisation (Vector)"
  ( TestCase
  $ let minimise w = VG.floydWarshall [1, 2, 3, 4] (`lookup` w)
    in  assertEqual
          ("floydWarshall should not change" ++ " already minimised graphs")
          weightsTC2APSP
          (minimise weightsTC2)
  )
