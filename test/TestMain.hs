module Main where

import qualified Fuzzing.FloydWarshall
import qualified QCGraph
import qualified QCStn
import           Test.Framework

main :: IO ()
main =
  defaultMain (QCGraph.tests ++ QCStn.tests ++ Fuzzing.FloydWarshall.tests)
