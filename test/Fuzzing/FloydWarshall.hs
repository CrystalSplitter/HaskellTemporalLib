module Fuzzing.FloydWarshall where

import           Prelude                 hiding ( lookup )

import           Control.Monad                  ( forM_ )
import           Data.Map.Strict                ( Map
                                                , fromList
                                                , lookup
                                                , toList
                                                )
import qualified System.Random                 as R
import qualified Test.Framework                as Framework
import           Test.Framework.Providers.HUnit ( hUnitTestToTests )
import           Test.HUnit                    as HUnit
                                                ( (@?)
                                                , Test(..)
                                                )

import           HaskellTemporalLib.Internal.Graph.Graph
                                               as G
                                                ( floydWarshall )

import           HaskellTemporalLib.Internal.Graph.Mut.VectorGraph
                                               as VG
                                                ( floydWarshall )
tests :: [Framework.Test]
tests = hUnitTestToTests $ TestList [floydWarshallVGFuzz, floydWarshallGFuzz]

tolerance :: (Floating a) => a
tolerance = 10.0 ** (-6.0)

realInTol :: (RealFloat a) => a -> a -> Bool
realInTol x y | isInfinite x && isInfinite y = True
              | otherwise                    = abs (x - y) <= tolerance

randomEdgeList
  :: (R.RandomGen g) => Int -> Int -> g -> [((Int, Int), Double)]
randomEdgeList numVerts numEdges gen
  | numEdges == 0
  = []
  | srcNode == tarNode
  = ((srcNode, tarNode), 0.0) : randomEdgeList numVerts (numEdges - 1) ngen2
  | otherwise
  = ((srcNode, tarNode), edgeWeight)
    : randomEdgeList numVerts (numEdges - 1) ngen3
 where
  (srcNode   , ngen1) = R.randomR (1, numVerts) gen
  (tarNode   , ngen2) = R.randomR (1, numVerts) ngen1
  (edgeWeight, ngen3) = R.randomR (0.0, 100.0) ngen2

randomWeights :: Map (Int, Int) Double
randomWeights = fromList $ randomEdgeList 50 100 (R.mkStdGen 42)

floydWarshallVGFuzz :: Test
floydWarshallVGFuzz = TestLabel
  "VectorGraph FloydWarshallFuzz1"
  ( TestCase
  $ let minimise w = VG.floydWarshall [1 .. 50] (`lookup` w)
        once  = toList (minimise randomWeights)
        twice = toList (minimise $ minimise randomWeights)
    in  forM_ (zip once twice) $ \(g1@(_, w1), g2@(_, w2)) -> do
          realInTol w1 w2 @? show g1 ++ " should be equal to " ++ show g2
  )

floydWarshallGFuzz :: Test
floydWarshallGFuzz = TestLabel
  "Graph FloydWarshallFuzz1"
  ( TestCase
  $ let minimise w = G.floydWarshall [1 .. 50] (`lookup` w)
        once  = toList (minimise randomWeights)
        twice = toList (minimise $ minimise randomWeights)
    in  forM_ (zip once twice) $ \(g1@(_, w1), g2@(_, w2)) -> do
          realInTol w1 w2 @? show g1 ++ " should be equal to " ++ show g2
  )
