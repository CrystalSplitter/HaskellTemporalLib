module Main where

import Test.Framework
import qualified QCIFPC

main :: IO ()
main = defaultMain QCIFPC.tests
