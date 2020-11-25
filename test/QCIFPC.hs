module QCIFPC where

import qualified Test.Framework as Framework (Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import qualified Test.HUnit as HUnit

import Data.Maybe
import Data.Map.Strict (Map, fromList)

import HaskellTemporalLib.IFPC (ifpc)

verts :: [Int]
verts = [1,2,3]

weights :: Map (Int, Int) Double
weights = fromList [ ((1, 2), 14.0)
                   , ((3, 1), 3.0)
                   , ((3, 2), 17.0)
                   , ((1, 3), 7.0)
                   , ((2, 3), 3.0)
                   , ((2, 1), 10.0)
                   ]

tcase1 :: HUnit.Test
tcase1 = HUnit.TestLabel "Identity"
  (HUnit.TestCase
    (HUnit.assertEqual "Change detected when none should exist"
      (fromJust (ifpc ((1, 2), 14.0) verts weights)) weights
    )
  )

tcase2 :: HUnit.Test
tcase2 = HUnit.TestLabel "Relaxed"
  (HUnit.TestCase
    (HUnit.assertEqual "Change detected when none should exist"
      (fromJust (ifpc ((1, 2), 17.0) verts weights)) weights
    )
  )

tcase3 :: HUnit.Test
tcase3 = HUnit.TestLabel "Tighten"
  (HUnit.TestCase
    (HUnit.assertBool "Change detected when some should exist"
      (fromJust (ifpc ((1, 2), 10.0) verts weights) /= weights)
    )
  )

tests :: [Framework.Test]
tests = hUnitTestToTests $ HUnit.TestList [ tcase1
                                          , tcase2
                                          , tcase3]
