module Main where

import qualified Fuzzing.FloydWarshall
import qualified Fuzzing.FuzzyIFPC
import qualified QCGraph
import qualified QCStn
import           Test.Framework

main :: IO ()
main = defaultMain $ concat
  [ QCGraph.tests
  , QCStn.tests
  , Fuzzing.FloydWarshall.tests
  , Fuzzing.FuzzyIFPC.tests
  ]
