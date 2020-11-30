{-# LANGUAGE TupleSections #-}

module HaskellTemporalLib.Internal.Stn
  ( SimpleTemporalNetwork(..)
  , ModifiableNet(..)
  , constrain
  , inf
  , Constraint
  , Bidirectional
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

-- | Represents a constraint between two events with weight w.
type Constraint e w = ((e, e), w)

showBCnst :: (Show a, Fractional a) => Bidirectional a -> String
showBCnst (rev, fwd) =
  let f c = fromMaybe inf c
  in  "[" ++ (show . negate . f) rev ++ "," ++ (show . f) fwd ++ "]"


-- | The constant, positive Infinity.
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

  allZConstraints :: (Ord event, Fractional b) => f event b -> [(event, Bidirectional b)]
  allZConstraints n = (\e -> (e, zBcnst e n)) <$> events n

  printer :: (Show a, Show b, Ord a, Ord b, Fractional b) => f a b -> [String]
  printer n = let zcns = Data.List.sortOn (fmap negate . fst . snd) $ allZConstraints n
                  formatter (e, b) = show e ++ " in " ++ showBCnst b
               in fmap formatter zcns

  printAllZConstraints :: (Show a, Show b, Ord a, Ord b, Fractional b) => f a b -> IO ()
  printAllZConstraints n = do
    mapM_ putStrLn $ printer n

  dependentEvents :: (Ord a, Fractional b) => a -> f a b -> [Constraint a b]
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
  -- where is the LAST event which is part of a solution which has the shortest makespan.
  earliestMakespan
    :: (Ord event, Ord b, Fractional b)
    => f event b
    -- ^ SimpleTemporalNetwork to generate an earliestMakespan from.
    -> (event, Bidirectional b)
    -- ^ A tuple holding the event which represents the earliest makespan, and the Bidirectional
    -- constraint binding that event to the Z-event.
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
  fromList :: (Ord a, Ord b) => a -> [Constraint a b] -> f a b
  fromList z xs = fromMap z $ M'.fromList xs

  -- | Build an STN object from a Map.
  fromMap :: (Ord a, Ord b) => a -> M'.Map (a, a) b -> f a b
  fromMap z m = fromList z $ M.toList m

  -- | Convert the STN to a strict Map object.
  toMap :: f a b -> M'.Map (a, a) b

class (SimpleTemporalNetwork f) => ModifiableNet f where
  -- | Sets the constraint from the first event to the second event.
  -- setCnst fromEvent toEvent value stn
  setCnst :: (Ord event) => event -> event -> weight -> f event weight -> f event weight

  -- | Sets a Bidirectional constraint (two constraints) in the provided STN.
  setBcnst :: (Ord event, Fractional b) => event -> event -> b -> b -> f event b -> f event b
  setBcnst fr to minVal maxVal n = setCnst fr to maxVal $ setCnst to fr (-minVal) n


-- | Create constraints which bind the two given nodes with min
-- and max bounds.
constrain
  :: (Num b)
  => node   -- ^ First event
  -> node   -- ^ Second event
  -> b      -- ^ Minimum time between first and second events
  -> b      -- ^ Maximum time between first and second events
  -> [Constraint node b]  -- ^ Generated constraints which imply these bounds.
constrain fr to minVal maxVal = [((to, fr), negate minVal), ((fr, to), maxVal)]


-- | Utility function to enumerate nC2 items.
pairings :: [a] -> [(a, a)]
pairings []       = []
pairings (x : xs) = [ (x, other) | other <- xs ] ++ pairings xs




{-
  ==============================================================================
-}

