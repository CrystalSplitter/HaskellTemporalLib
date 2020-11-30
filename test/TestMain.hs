module Main where

import Test.Framework
import qualified QCGraph
import qualified QCStn

main :: IO ()
main = defaultMain (QCGraph.tests ++ QCStn.tests)
