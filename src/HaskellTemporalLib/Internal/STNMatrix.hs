module HaskellTemporalLib.Internal.STNMatrix where

import qualified Data.Map.Strict               as M'
import qualified Data.Vector                   as V
import           HaskellTemporalLib.Internal.Graph.Matrix
                                                ( Matrix(..)
                                                , at
                                                , idxAt
                                                )
import           HaskellTemporalLib.Internal.Stn

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | An STN with internal data structured as a matrix.
data STNMatrix e t = STNMatrix
  { getZEvent        :: e
  , getIdxToEventVec :: V.Vector e
  , matData          :: Matrix (V.Vector t)
  }
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

eventToIdxMap
  :: (Ord event, Num idx, Enum idx) => STNMatrix event t -> M'.Map event idx
eventToIdxMap net =
  M'.fromList (zip (V.toList $ getIdxToEventVec net) [0 ..])

getIdxOfEvent :: (Ord k, Num a, Enum a) => STNMatrix k t -> k -> a
getIdxOfEvent net e = eventToIdxMap net M'.! e

------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

instance SimpleTemporalNetwork STNMatrix where
  zEvent net = getZEvent net
  events net = V.toList $ getIdxToEventVec net
  cnst i j net =
    Just $ matData net `at` (getIdxOfEvent net i, getIdxOfEvent net j)
  toMap net = M'.fromList
    [ ((e1, e2), matData net `at` (i1, i2))
    | (i1, e1) <- zip [0 ..] (events net)
    , (i2, e2) <- zip [0 ..] (events net)
    ]
  toMatrix = matData

instance ModifiableNet STNMatrix where

  setCnst fr to w net =
    let m       = matData net
        frIdx   = getIdxOfEvent net fr
        toIdx   = getIdxOfEvent net to
        newData = m { dat = dat m V.// [(m `idxAt` (frIdx, toIdx), w)] }
    in  net { matData = newData }

  setBcnst fr to w1 w2 net =
    let m       = matData net
        frIdx   = getIdxOfEvent net fr
        toIdx   = getIdxOfEvent net to
        newData = m
          { dat = dat m
                    V.// [ (m `idxAt` (frIdx, toIdx), w2)
                         , (m `idxAt` (toIdx, frIdx), negate w1)
                         ]
          }
    in  net { matData = newData }
