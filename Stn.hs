{-# LANGUAGE TupleSections #-}

module Stn where

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           GHC.Real                      as Real
import           Data.Map                      as M
import           Data.Maybe
import           Data.Sequence                 as S
import           Data.List

-- | Constraint, mapping from a start time to and end time,
-- Representing t_e - t_s <= Cse
--
-- >>> let from = pack "Start Time"
-- >>> let to = pack "End Time"
-- >>> LinCnst from to 5.0
class Constraint f where
  value :: (Num b) => f a b -> b
  source :: f a b -> a
  target :: f a b -> a
  nullConstraint :: (Fractional b) => a -> a -> f a b

data LinCnst a b = LinCnst a a b deriving (Eq, Show)

instance Constraint LinCnst where
  value (LinCnst _ _ c) = c
  source (LinCnst fr _ _) = fr
  target (LinCnst _ to _) = to
  nullConstraint fr to = LinCnst fr to (1.0 / 0.0)

class SimpleTemporalNetwork f where
  -- | The origin (Z) event.
  zEvent :: f a b -> a
  -- | The list of events in the STN.
  events :: (Eq a) => f a b -> [a]
  -- | The uni-directional constaint from the first
  -- argument to the second.
  cnst :: (Ord a, Fractional b) => a -> a -> f a b -> Maybe b
  -- | The bi-directional constaint pair.
  -- First element is the reverse, second is the forward.
  bcnst :: (Ord a, Fractional b) => a -> a -> f a b -> (Maybe b, Maybe b)
  bcnst fr to n = (cnst to fr n, cnst fr to n)

  dependentEvents :: (Ord a, Fractional b) => a -> f a b -> [((a, a), b)]
  dependentEvents fr n = let cnst' fr to n = if fr == to then Nothing else cnst fr to n
                             binding to = ((fr, to),) <$> cnst' fr to n
                          in catMaybes $ binding <$> events n

data STNMap a b = STNMap { getZEvent :: a, getMap :: M.Map (a, a) b } deriving (Show)


-- | Build an STNMap object from a list.
-- If multiple constraints are specified for the same event pairs, then the tighter one
-- is stored in the network.
stnMapFromList :: (Ord a, Ord b) => a -> [((a, a), b)] -> STNMap a b
stnMapFromList z xs = STNMap { getZEvent = z, getMap = builtMap }
  where builtMap = M.fromListWith (\new old -> min new old) xs


instance SimpleTemporalNetwork STNMap where
  zEvent n = getZEvent n
  events n = uniqueEvents
   where
    eventGroups  = (\(x, y) -> [x, y]) <$> (M.keys . getMap) n
    uniqueEvents = (nub . concat) eventGroups
  cnst fr to n = if fr == to then Just 0.0 else M.lookup (fr, to) $ getMap n


constrain :: (Num b) => a -> a -> b -> b -> [((a, a), b)]
constrain fr to minVal maxVal = [((to, fr), negate minVal), ((fr, to), maxVal)]

test = STNMap
  { getZEvent = 'z'
  , getMap    = M.fromList
                $  constrain 'z' 'a' 0.0 10.0
                ++ constrain 'z' 'b' 0.0 10.0
                ++ constrain 'a' 'b' 2.0 7.0
  }


inf :: (Fractional a) => a
inf = 1.0 / 0.0

-- | Generate an all-pairs shortest-path mapping between any two nodes of
-- type `a`, with distance being of type `d`.
--
-- `e` is the list of nodes to schedule.
-- `w` is a function which returns the weights between any two events.
floydWarshall
  :: (Ord a, Ord d, Fractional d)
  => [a]
  -> ((a, a) -> Maybe d)
  -> M.Map (a, a) d
floydWarshall e w = floydWarshallRec eventGroups w
 where
  eventGroups = [ (i, j, k) | k <- e, i <- e, j <- e ]
  floydWarshallRec
    :: (Ord a, Ord d, Fractional d)
    => [(a, a, a)]
    -> ((a, a) -> Maybe d)
    -> M.Map (a, a) d
  floydWarshallRec [] _ = mempty
  floydWarshallRec ((i, j, k) : es) w' =
    M.insert (i, j) distUpdated $ previousWeights
   where
    distUpdated = min (getDist i j) (getDist i k + getDist k j)
    getDist x y =
      fromMaybe (fromMaybe inf $ w' (x, y)) $ previousWeights M.!? (x, y)
    previousWeights = floydWarshallRec es w'


minimiseNetwork
  :: (Ord a, SimpleTemporalNetwork n, Ord d, Fractional d)
  => n a d
  -> STNMap a d
minimiseNetwork stn = STNMap
  { getZEvent = zEvent stn
  , getMap    = floydWarshall (events stn) (\(x, y) -> cnst x y stn)
  }

-- | Utility function to enumerate nC2 items.
pairings :: [a] -> [(a, a)]
pairings []       = []
pairings (x : xs) = [ (x, other) | other <- xs ] ++ pairings xs

isConsistent
  :: (SimpleTemporalNetwork n, Ord a, Fractional d, Ord d) => n a d -> Bool
isConsistent net = all (uncurry isOkay) pairs
 where
  pairs = pairings $ events net
  cnst' i j = fromMaybe inf $ cnst i j net
  isOkay i j = cnst' i j >= (-cnst' j i)
