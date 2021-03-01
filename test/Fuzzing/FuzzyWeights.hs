module Fuzzing.FuzzyWeights where

import           Data.Map.Strict                ( Map
                                                , fromList
                                                )
import           System.Random                  ( RandomGen
                                                , mkStdGen
                                                , randomR
                                                )

randomEdgeList :: (RandomGen g) => Int -> Int -> g -> [((Int, Int), Double)]
randomEdgeList numVerts numEdges gen
  | numEdges == 0
  = []
  | srcNode == tarNode
  = ((srcNode, tarNode), 0.0) : randomEdgeList numVerts (numEdges - 1) ngen2
  | otherwise
  = ((srcNode, tarNode), edgeWeight)
    : randomEdgeList numVerts (numEdges - 1) ngen3
 where
  (srcNode   , ngen1) = randomR (1, numVerts) gen
  (tarNode   , ngen2) = randomR (1, numVerts) ngen1
  (edgeWeight, ngen3) = randomR (0.0, 100.0) ngen2

-- | Generate a random weight mapping.
randWeights
  :: (RandomGen g)
  => Int -- ^ Number of verts.
  -> Int -- ^ Number of edges.
  -> g -- ^ Random number generator.
  -> ([Int], Map (Int, Int) Double) -- ^ Output mapping
randWeights nVerts nEdges g =
  ([1 .. nVerts], fromList $ randomEdgeList nVerts nEdges g)

-- | Same as @randweights@ but with an integer seed.
randWeightsSeed
  :: Int -- ^ Number of verts.
  -> Int -- ^ Number of edges.
  -> Int -- ^ Random number generator seed.
  -> ([Int], Map (Int, Int) Double) -- ^ Output mapping.
randWeightsSeed a b seed = randWeights a b $ mkStdGen seed

-- | Generate a single random weight.
randWeight
  :: (RandomGen g)
  => Int -- ^ Number of verts.
  -> g -- ^ Random number generator.
  -> ((Int, Int), Double)
randWeight nVerts gen =
  let (srcNode, ngen1) = randomR (1, nVerts) gen
      (tarNode, ngen2) = randomR (1, nVerts) ngen1
      (w      , ngen3) = randomR (0.0, 100.0) ngen2
  in  if srcNode == tarNode
        then randWeight nVerts ngen3
        else ((srcNode, tarNode), w)

randWeightSeed :: Int -> Int -> ((Int, Int), Double)
randWeightSeed a seed = randWeight a $ mkStdGen seed
