module Fuzzing.FloydWarshall where

import           Prelude                 hiding ( lookup )

import           Control.Monad                  ( forM_ )
import           Data.Map.Strict                ( Map
                                                , lookup
                                                , toList
                                                )
import           Fuzzing.FuzzyWeights           ( randWeightsSeed )
import qualified Test.Framework                as Framework
import           Test.Framework.Providers.HUnit ( hUnitTestToTests )
import           Test.HUnit                    as HUnit
                                                ( (@?)
                                                , Test(..)
                                                )
import           TestUtils

import           HaskellTemporalLib.Internal.Graph.Graph
                                               as G
                                                ( floydWarshall )

import           HaskellTemporalLib.Internal.Graph.Mut.VectorGraph
                                               as VG
                                                ( floydWarshall )
tests :: [Framework.Test]
tests = hUnitTestToTests $ TestList [floydWarshallVGFuzz, floydWarshallGFuzz]

randomWeights :: ([Int], Map (Int, Int) Double)
randomWeights = randWeightsSeed 50 200 42

floydWarshallVGFuzz :: Test
floydWarshallVGFuzz = TestLabel
  "VectorGraph FloydWarshallFuzz1"
  ( TestCase
  $ let minimise w = VG.floydWarshall (fst randomWeights) (`lookup` w)
        once  = toList (minimise $ snd randomWeights)
        twice = toList (minimise $ minimise $ snd randomWeights)
    in  forM_ (zip once twice) $ \(g1@(_, w1), g2@(_, w2)) -> do
          assertApprox (show g1 ++ " should be equal to " ++ show g2) w1 w2
  )

floydWarshallGFuzz :: Test
floydWarshallGFuzz = TestLabel
  "Graph FloydWarshallFuzz1"
  ( TestCase
  $ let minimise w = G.floydWarshall [1 .. 50] (`lookup` w)
        once  = toList (minimise $ snd randomWeights)
        twice = toList (minimise $ minimise $ snd randomWeights)
    in  forM_ (zip once twice) $ \(g1@(_, w1), g2@(_, w2)) -> do
          realInTol w1 w2 @? show g1 ++ " should be equal to " ++ show g2
  )
