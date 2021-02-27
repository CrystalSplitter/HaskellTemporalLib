{-# LANGUAGE TupleSections #-}

module HaskellTemporalLib.Internal.Schedules
  ( assignEvent
  , smallestMakespanSchedule
  )
where

import HaskellTemporalLib.Internal.Stn
  ( SimpleTemporalNetwork(..)
  , ModifiableNet(..)
  )
import HaskellTemporalLib.Internal.MinimalSTN (minimise, generalise)
import HaskellTemporalLib.Internal.Graph.Graph (ifpc)


-- | Assign an event in an STN.
assignEvent
  :: (ModifiableNet n, Ord event, Fractional t)
  => event      -- ^ Event to assign in n.
  -> t          -- ^ Time to assign it, relative to the Z event.
  -> n event t  -- ^ ModifiableNet to assign the event in.
  -> n event t  -- ^ New modifiable net with the assigned event.
assignEvent e val stn = setBcnst (zEvent stn) e val val stn

-- | Return a schedule (i.e. all events have been assigned), such that
-- the event which must occur last temporally is forced to occur as early as
-- possible.
--
-- The times of events besides the last event are chosen arbitrarily
-- while meeting this constraint.
smallestMakespanSchedule
  :: (ModifiableNet n, Ord event, Ord t, Fractional t)
  => n event t
  -> Maybe (n event t)
smallestMakespanSchedule n = go $ smsFirstIter n
 where
  go
    :: (Ord a, Ord d, Fractional d, SimpleTemporalNetwork n)
    => Maybe (n a d)
    -> Maybe (n a d)
  go Nothing = Nothing
  go (Just stn)
    -- If all events have fixed times, then we're done!
    | null $ unassignedEvents stn = return stn
    -- Still some events to assign.
    | otherwise = go updatedStn
    where
      nextUnassigned         = head $ unassignedEvents stn
      nextUnassignedMinBound = negate <$> fst (zBcnst nextUnassigned stn)
      newConstraintM = ((zEvent stn, nextUnassigned),) <$> nextUnassignedMinBound
      -- The lines below here can be replaced with any suitable
      -- partial minimasation.
      newWeights = newConstraintM >>= \x -> ifpc x (events stn) (toMap stn)
      updatedStn = fromMap (zEvent stn) <$> newWeights


smsFirstIter
  :: (ModifiableNet n, Ord event, Ord t, Fractional t)
  => n event t
  -> Maybe (n event t)
smsFirstIter m =
  let miniM = minimise m
      mkspanM x = fmap negate (fst $ snd $ earliestMakespan x)
      newConstraintM = miniM >>= mkspanM
      finalEvent x = fst $ earliestMakespan x
      -- Define some wrappers around assignEvent so it be used with Maybes
      updateStn Nothing  _        = Nothing
      updateStn _        Nothing  = Nothing
      updateStn (Just c) (Just n) = Just $ assignEvent (finalEvent n) c n
  in  updateStn newConstraintM $ generalise <$> miniM
