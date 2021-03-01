module Fuzzing.FuzzyIFPC where

import           Control.Monad
import qualified HaskellTemporalLib.Internal.Graph.Mut.VectorGraph
                                               as VG
import qualified Test.Framework                as Framework
import           Test.Framework.Providers.HUnit ( hUnitTestToTests )
import           Test.HUnit                    as HUnit
                                                ( Test(..) )

import qualified Data.Map.Strict               as M'
import           Fuzzing.FuzzyWeights
import           TestUtils

tests :: [Framework.Test]
tests = hUnitTestToTests $ TestList [minCompare]

minCompare :: Test
minCompare = TestLabel "Comparing " $ TestCase $ do
  forM_ [0 .. 10] $ \i -> do
    let (verts, startingWeights) = randWeightsSeed 50 200 i
        minimise w = VG.floydWarshall verts (`M'.lookup` w)
        firstMinimalForm = minimise startingWeights
        addedEdge        = randWeightSeed 50 $ i + 100
        secondMinimalForm =
          minimise $ uncurry (M'.insertWith min) addedEdge firstMinimalForm
        minimiseIFPC w = VG.ifpc addedEdge verts w
    print addedEdge
    forM_ (M'.keys secondMinimalForm) $ \key -> do
      maybe
        (pure ())
        (\newWeights -> do
          assertApprox ("Key: " ++ show key)
                       (secondMinimalForm M'.! key)
                       (newWeights M'.! key)
        )
        (minimiseIFPC firstMinimalForm)
