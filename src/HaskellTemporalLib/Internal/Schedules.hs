{-# LANGUAGE TupleSections #-}

module HaskellTemporalLib.Internal.Schedules (assignEvent, smallestMakespanSchedule) where

import HaskellTemporalLib.Internal.Stn
  ( SimpleTemporalNetwork(..)
  , ModifiableNet(..)
  )
import HaskellTemporalLib.MinimalSTN (minimiseNetwork, generalise)
import HaskellTemporalLib.Graph (ifpc)


-- | Assign an event in an STN.
assignEvent
  :: (ModifiableNet n, Ord a, Fractional d)
  => a
  -> d
  -> n a d
  -> n a d
assignEvent e val stn = setBcnst (zEvent stn) e val val stn


smallestMakespanSchedule
  :: (ModifiableNet n, Ord a, Ord d, Fractional d)
  => n a d
  -> Maybe (n a d)
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
      newConstraintM = ((nextUnassigned, zEvent stn),) <$> nextUnassignedMinBound
      -- The lines below here can be replaced with any suitable
      -- partial minimasation.
      newWeights = newConstraintM >>= \x -> ifpc x (events stn) (toMap stn)
      updatedStn = fromMap (zEvent stn) <$> newWeights


smsFirstIter
  :: (ModifiableNet n, Ord a, Ord d, Fractional d)
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
  in  updateStn newConstraintM $ generalise <$> miniM
