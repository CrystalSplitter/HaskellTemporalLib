module Main where

import Test.Framework
import qualified QCGraph

main :: IO ()
main = defaultMain QCGraph.tests
