{-# LANGUAGE TupleSections #-}

module HaskellTemporalLib.Stn
  ( STNMap(..)
  , SimpleTemporalNetwork(..)
  , constrain
  , inf
  , smallestMakespanSchedule
  )
where

import qualified Data.Map                      as M
import qualified Data.Map.Strict               as M'
import           Data.Maybe
import           Data.List

-- | Constraint, mapping from a start time to and end time,
--

-- | A Bidirectional Linear Constraint
-- This represents -Ces <= t_e - t_s <= Cse
type Bidirectional a = (Maybe a, Maybe a)


showBCnst :: (Show a, Fractional a) => Bidirectional a -> String
showBCnst (rev, fwd) =
  let f c = fromMaybe inf c
  in  "[" ++ (show . negate . f) rev ++ "," ++ (show . f) fwd ++ "]"


inf :: (Fractional a) => a
inf = 1.0 / 0.0


class SimpleTemporalNetwork f where
  -- | The origin (Z) event.
  zEvent :: f event b -> event
  -- | The list of events in the STN.
  events :: (Eq event) => f event b -> [event]
  -- | The uni-directional constaint from the first.
  -- argument to the second.
  cnst :: (Ord event, Fractional b) => event -> event -> f event b -> Maybe b
  -- | The bi-directional constaint pair.
  -- First element is the reverse, second is the forward.
  bcnst :: (Ord event, Fractional b) => event -> event -> f event b -> Bidirectional b
  bcnst fr to n = (cnst to fr n, cnst fr to n)
  -- | The bi-directional Z-constraint pair.
  -- First element is the event -> z edge, second is the z -> event edge.
  zBcnst :: (Ord event, Fractional b) => event -> f event b -> Bidirectional b
  zBcnst e n = bcnst (zEvent n) e n

  -- | Sets the constraint from the first event to the second event.
  -- setCnst fromEvent toEvent value stn
  setCnst :: (Ord event) => event -> event -> b -> f event b -> f event b

  setBcnst :: (Ord event, Fractional b) => event -> event -> b -> b -> f event b -> f event b
  setBcnst fr to minVal maxVal n = setCnst fr to maxVal $ setCnst to fr (-minVal) n

  allZConstraints :: (Ord event, Fractional b) => f event b -> [(event, Bidirectional b)]
  allZConstraints n = (\e -> (e, zBcnst e n)) <$> events n

  printer :: (Show a, Show b, Ord a, Ord b, Fractional b) => f a b -> [String]
  printer n = let zcns = Data.List.sortOn (fmap negate . fst . snd) $ allZConstraints n
                  formatter (e, b) = show e ++ " in " ++ showBCnst b
               in fmap formatter zcns

  printAllZConstraints :: (Show a, Show b, Ord a, Ord b, Fractional b) => f a b -> IO ()
  printAllZConstraints n = do
    mapM_ putStrLn $ printer n

  dependentEvents :: (Ord a, Fractional b) => a -> f a b -> [((a, a), b)]
  dependentEvents fr n = let cnst' t = if fr == t then Nothing else cnst fr t n
                             binding to = ((fr, to),) <$> cnst' to
                          in catMaybes $ binding <$> events n

  -- | Return a Maybe with the event's fixed time. Nothing if the event is not fixed.
  eventTime :: (Ord event, Fractional b, Eq b) => event -> f event b -> Maybe b
  eventTime e n = let (rev, fwd) = zBcnst e n
                      revEvaled = fromMaybe inf rev
                      fwdEvaled = fromMaybe inf fwd
                   in if (-revEvaled) == fwdEvaled then Just fwdEvaled else Nothing

  -- | Return a tuple of the form (e, (Maybe reverse constraint, Maybe forward constraint))
  -- where `e` is the LAST event which is part of a solution which has the shortest makespan.
  earliestMakespan
    :: (Ord event, Ord b, Fractional b) => f event b -> (event, Bidirectional b)
  earliestMakespan n = Data.List.foldr f (zEvent n, (Just inf, Just (-inf))) $ allZConstraints n
   where
    f arg1@(_, (rev1, _)) arg2@(_, (rev2, _)) =
      if rev2 < rev1 then arg2 else arg1

  -- | Return every event that has no assigned, fixed time.
  unassignedEvents :: (Ord a, Eq b, Fractional b) => f a b -> [a]
  unassignedEvents n
    | events n == mempty = mempty
    | otherwise = Data.List.filter (isNothing . flip eventTime n) $ events n

  -- | Return whether or not this STN is consistent.
  isConsistent :: (Ord a, Fractional b, Ord b) => f a b -> Bool
  isConsistent net = all (uncurry isOkay) pairs
   where
    pairs = pairings $ events net
    cnst' i j = fromMaybe inf $ cnst i j net
    isOkay i j = cnst' i j >= (-cnst' j i)

  -- | Build an STN object from a list.
  -- If multiple constraints are specified for the same event pairs, then the tighter one
  -- is stored in the network.
  fromList :: (Ord a, Ord b) => a -> [((a, a), b)] -> f a b

  -- | Build an STN object from a Map.
  fromMap :: (Ord a, Ord b) => a -> M.Map (a, a) b -> f a b
  fromMap z m = fromList z $ M.toList m

  -- | Minimise a Simple Temporal Network
  minimiseNetwork
    :: (Ord a, Ord d, Fractional d)
    => f a d
    -> Maybe (f a d)
  minimiseNetwork stn = if isConsistent newStn then Just newStn else Nothing
   where
    newMap = floydWarshall (events stn) (\(x, y) -> cnst x y stn)
    newStn = fromMap (zEvent stn) newMap


data STNMap a b = STNMap { getZEvent :: a, mapData :: M.Map (a, a) b } deriving (Show)


instance SimpleTemporalNetwork STNMap where
  zEvent n = getZEvent n
  events n = uniqueEvents
   where
    uniqueEvents = nub . concat $ eventGroups
    eventGroups  = (\(x, y) -> [x, y]) <$> (M.keys . mapData) n
  cnst fr to n = if fr == to then Just 0.0 else M.lookup (fr, to) $ mapData n
  setCnst fr to val n = STNMap { getZEvent = getZEvent n, mapData = newMap }
    where newMap = M.insert (fr, to) val $ mapData n
  fromList z xs = STNMap { getZEvent = z, mapData = builtMap }
    where builtMap = M.fromListWith min xs
  fromMap z m = STNMap { getZEvent = z, mapData = m }


constrain :: (Num b) => node -> node -> b -> b -> [((node, node), b)]
constrain fr to minVal maxVal = [((to, fr), negate minVal), ((fr, to), maxVal)]


-- | Generate an all-pairs shortest-path mapping between any two nodes.
--
-- `e` is the list of nodes to schedule.
-- `w` is a function which returns the weights between any two events.
-- >>> floydwarshall eventList weightFunction
floydWarshall
  :: (Ord node, Ord dist, Fractional dist)
  => [node]
  -> ((node, node) -> Maybe dist)
  -> M.Map (node, node) dist
floydWarshall e = floydWarshallRec eventGroups
 where
  eventGroups = [ (i, j, k) | k <- e, i <- e, j <- e ]
  floydWarshallRec
    :: (Ord node, Ord dist, Fractional dist)
    => [(node, node, node)]
    -> ((node, node) -> Maybe dist)
    -> M.Map (node, node) dist
  floydWarshallRec []               _  = mempty
  floydWarshallRec ((i, j, k) : es) w' = M.insert (i, j)
                                                  distUpdated
                                                  previousWeights
   where
    distUpdated = min (getDist i j) (getDist i k + getDist k j)
    getDist x y =
      fromMaybe (fromMaybe inf (w' (x, y))) (previousWeights M.!? (x, y))
    previousWeights = floydWarshallRec es w'


-- | Utility function to enumerate nC2 items.
pairings :: [a] -> [(a, a)]
pairings []       = []
pairings (x : xs) = [ (x, other) | other <- xs ] ++ pairings xs


assignEvent
  :: (SimpleTemporalNetwork n, Ord a, Fractional d)
  => a
  -> d
  -> n a d
  -> n a d
assignEvent e val stn = setBcnst (zEvent stn) e val val stn


smallestMakespanSchedule
  :: (SimpleTemporalNetwork n, Ord a, Ord d, Fractional d)
  => n a d
  -> Maybe (n a d)
smallestMakespanSchedule n = go $ smsFirstIter n
 where
  go
    :: (Ord a, Ord d, Fractional d, SimpleTemporalNetwork n)
    => Maybe (n a d)
    -> Maybe (n a d)
  go Nothing = Nothing
  go (Just stn) |
    -- If all events have fixed times, then we're done!
                  unassignedEvents stn == mempty = return stn
                |
    -- Still some events to assign.
                  otherwise = go $ updateAllEdges =<< updatedStn
   where
    updatedStn =
      (\x -> assignEvent nextUnassigned x stn) <$> nextUnassignedMinBound
    nextUnassigned         = head $ unassignedEvents stn
    nextUnassignedMinBound = negate <$> fst (zBcnst nextUnassigned stn)
    -- Function alias for minimisation. We should replace this with the O(n^2)
    -- algorithm.
    updateAllEdges         = minimiseNetwork


smsFirstIter
  :: (SimpleTemporalNetwork n, Ord a, Ord d, Fractional d)
  => n a d
  -> Maybe (n a d)
smsFirstIter m =
  let miniM = minimiseNetwork m
      mkspanM x = fmap negate (fst $ snd $ earliestMakespan x)
      newConstraintM = miniM >>= mkspanM
      finalEvent x = fst $ earliestMakespan x
      -- Define some wrappers around assignEvent so it be used with Maybes
      updateStn Nothing  _        = Nothing
      updateStn _        Nothing  = Nothing
      updateStn (Just c) (Just n) = Just $ assignEvent (finalEvent n) c n
  in  updateStn newConstraintM miniM


{-
  ==============================================================================
-}

test =
  fromList 'z'
  $  constrain 'z' 'a' 10.0    10.0
  ++ constrain 'a' 'b' 200.0   200.0
  ++ constrain 'b' 'c' 300.0   3000.0
  ++ constrain 'c' 'd' 40000.0 40000.0 :: STNMap Char Double
