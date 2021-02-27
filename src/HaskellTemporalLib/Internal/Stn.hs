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

-- | A Bidirectional Linear Constraint.
--
-- This represents \( -C_{es} <= t_e - t_s <= C_{se} \)
type Bidirectional t = (Maybe t, Maybe t)

-- | Represents a constraint between two events with weight t.
type Constraint e t = ((e, e), t)

showBCnst :: (Show a, Fractional a) => Bidirectional a -> String
showBCnst (rev, fwd) =
  let f c = fromMaybe inf c
  in  "[" ++ (show . negate . f) rev ++ "," ++ (show . f) fwd ++ "]"


-- | The constant, positive Infinity.
inf :: (Fractional a) => a
inf = 1.0 / 0.0


class SimpleTemporalNetwork net where

  -- | The origin (Z) event.
  zEvent :: net event t -> event

  -- | The list of events in the STN.
  events :: (Eq event) => net event t -> [event]

  -- | The uni-directional constraint from the first argument to the second.
  cnst
    :: (Ord event, Fractional t)
    => event -- ^ Source event.
    -> event -- ^ Target event.
    -> net event t -- ^ STN to retrieve this information from.
    -> Maybe t  -- ^ The time constraint between the two events, if it exists.

  -- | The bi-directional constaint pair.
  -- First element is the reverse, second is the forward.
  bcnst
    :: (Ord event, Fractional t)
    => event
    -> event
    -> net event t
    -> Bidirectional t
  bcnst fr to n = (cnst to fr n, cnst fr to n)

  -- | The bi-directional Z-constraint pair.
  -- First element is the event -> z edge, second is the z -> event edge.
  zBcnst
    :: (Ord event, Fractional t)
    => event
    -> net event t
    -> Bidirectional t
  zBcnst e n = bcnst (zEvent n) e n

  -- | All constraints between every event and the Z event.
  -- This includes the Z event compared against itself,
  -- which will always return a zero-like constraint.
  allZConstraints
    :: (Ord event, Fractional t)
    => net event t
    -> [(event, Bidirectional t)]
  allZConstraints n = (\e -> (e, zBcnst e n)) <$> events n

  printer
    :: (Show event, Show t, Ord event, Ord t, Fractional t)
    => net event t
    -> [String]
  printer n
    = let
        zcns = Data.List.sortOn (fmap negate . fst . snd) (allZConstraints n)
        formatter (e, t) = show e ++ " in " ++ showBCnst t
      in fmap formatter zcns

  printAllZConstraints
    :: (Show event, Show t, Ord event, Ord t, Fractional t)
    => net event t
    -> IO ()
  printAllZConstraints n = do
    mapM_ putStrLn $ printer n

  dependentEvents
    :: (Ord event, Fractional t)
    => event
    -> net event t
    -> [Constraint event t]
  dependentEvents fr n
    = let cnst' t = if fr == t then Nothing else cnst fr t n
          binding to = ((fr, to),) <$> cnst' to
       in catMaybes $ binding <$> events n

  -- | Return a @Maybe@ with the event's fixed time.
  -- @Nothing@ if the event is not fixed.
  eventTime
    :: (Ord event, Fractional t, Eq t)
    => event
    -> net event t
    -> Maybe t
  eventTime e n = let (rev, fwd) = zBcnst e n
                      revEvaled = fromMaybe inf rev
                      fwdEvaled = fromMaybe inf fwd
                   in if (-revEvaled) == fwdEvaled
                        then Just fwdEvaled
                        else Nothing

  -- | Return a tuple of the form
  -- @(e, (Maybe reverse constraint, Maybe forward constraint))@
  -- where e is the LAST event which is part of a solution which has the
  -- shortest makespan.
  earliestMakespan
    :: (Ord event, Ord t, Fractional t)
    => net event t
    -- ^ SimpleTemporalNetwork to generate an earliestMakespan from.
    -> (event, Bidirectional t)
    -- ^ A tuple holding the event which represents the earliest makespan,
    -- and the Bidirectional constraint binding that event to the Z-event.
  earliestMakespan n
    = Data.List.foldr f startingConstraint (allZConstraints n)
   where
    startingConstraint = (zEvent n, (Just inf, Just (-inf)))
    f arg1@(_, (rev1, _)) arg2@(_, (rev2, _)) =
      if rev2 < rev1 then arg2 else arg1

  -- | Return every event that has no assigned, fixed time.
  unassignedEvents
    :: (Ord event, Eq t, Fractional t)
    => net event t
    -> [event]
  unassignedEvents n
    | events n == mempty = mempty
    | otherwise = Data.List.filter (isNothing . flip eventTime n) $ events n

  -- | Return whether or not this STN is consistent.
  isConsistent :: (Ord event, Fractional t, Ord t) => net event t -> Bool
  isConsistent net = all (uncurry isOkay) pairs
   where
    pairs = pairings $ events net
    cnst' i j = fromMaybe inf $ cnst i j net
    isOkay i j = cnst' i j >= (-cnst' j i)

  -- | Build an STN object from a list.
  -- If multiple constraints are specified for the same event pairs, then
  -- the tighter one is stored in the network.
  fromList
    :: (Ord event, Ord t)
    => event
    -> [Constraint event t]
    -> net event t
  fromList z xs = fromMap z $ M'.fromList xs

  -- | Build an STN object from a Map.
  fromMap
    :: (Ord event, Ord t)
    => event
    -> M'.Map (event, event) t
    -> net event t
  fromMap z m = fromList z $ M.toList m

  -- | Convert the STN to a strict Map object.
  toMap :: net event t -> M'.Map (event, event) t

class (SimpleTemporalNetwork net) => ModifiableNet net where
  -- | Sets the constraint from the first event to the second event.
  -- setCnst fromEvent toEvent value stn
  setCnst
    :: (Ord event)
    => event -- ^ Source event.
    -> event -- ^ Target event.
    -> weight -- ^ Maximum time allowed from Source to Target.
    -> net event weight -- ^ ModifiableNet to update.
    -> net event weight -- ^ The new ModifiableNet.

  -- | Sets a Bidirectional constraint (two constraints) in the provided STN.
  setBcnst
    :: (Ord event, Fractional t)
    => event -- ^ The source event.
    -> event -- ^ The target event.
    -> t -- ^ Minimum time allowed from Source to Target.
    -> t -- ^ Maximum time allowed from Source to Target.
    -> net event t -- ^ The ModifiableNet to update.
    -> net event t -- ^ The updated ModifiableNet.
  setBcnst fr to minVal maxVal n
    = setCnst fr to maxVal $ setCnst to fr (-minVal) n


-- | Create constraints which bind the two given nodes with min and
-- max bounds.
--
-- Say you want to represent that the soonest you can leave your home is
-- 30 minutes after you wake up, but the latest you can leave your home is
-- 2 hours after you wake up. We can represent this with the following:
--
-- >>> let c = constrain "Wake Up" "Leave Home" (30 * 60) (120 * 60)
-- >>> c
-- <BLANKLINE>
-- [(("Leave Home","Wake Up"),-1800),(("Wake Up","Leave Home"),7200)]
-- <BLANKLINE>
-- >>> fromList "Wake Up" c :: STNMap String Int
-- <BLANKLINE>
-- STNMap {getZEvent = "Wake Up", mapData = fromList [(("Leave Home","Wake Up"),-1800),(("Wake Up","Leave Home"),7200)]}
constrain
  :: (Num t)
  => node   -- ^ First event
  -> node   -- ^ Second event
  -> t      -- ^ Minimum time between first and second events
  -> t      -- ^ Maximum time between first and second events
  -> [Constraint node t]  -- ^ Generated constraints which imply these bounds.
constrain fr to minVal maxVal
  = [((to, fr), negate minVal), ((fr, to), maxVal)]


-- | Utility function to enumerate nC2 items.
pairings :: [a] -> [(a, a)]
pairings []       = []
pairings (x : xs) = [ (x, other) | other <- xs ] ++ pairings xs




{-
  ==============================================================================
-}

