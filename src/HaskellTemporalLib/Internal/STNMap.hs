module HaskellTemporalLib.Internal.STNMap (STNMap(..)) where

import Data.List (nub)
import qualified Data.Map as M
import HaskellTemporalLib.Internal.Stn
  ( SimpleTemporalNetwork(..)
  , ModifiableNet(..)
  )

data STNMap a b = STNMap { getZEvent :: a
                         , mapData :: M.Map (a, a) b
                         } deriving (Eq, Show)


instance SimpleTemporalNetwork STNMap where
  zEvent n = getZEvent n
  events n = uniqueEvents
   where
    uniqueEvents = nub . concat $ eventGroups
    eventGroups  = (\(x, y) -> [x, y]) <$> (M.keys . mapData) n
  cnst fr to n = if fr == to then Just 0.0 else M.lookup (fr, to) $ mapData n
  fromList z xs = STNMap { getZEvent = z, mapData = builtMap }
    where builtMap = M.fromListWith min xs
  fromMap z m = STNMap { getZEvent = z, mapData = m }
  toMap n = mapData n

instance ModifiableNet STNMap where
  setCnst fr to val n = n { mapData = newMap }
    where newMap = M.insert (fr, to) val $ mapData n
