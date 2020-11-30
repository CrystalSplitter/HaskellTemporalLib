module HaskellTemporalLib.Internal.Graph
( ifpc
, floydWarshall
, NewConstraint
) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M'

type NewConstraint v e = ((v, v), e)

{-|
  Incremental Full Path Consistency.

  Takes in a new constraint, a Foldable of vertices, and an
  all-pairs-shortest-path distance Map.

  Returns a newly minimised distance map if the new graph can be minimised.

  Léon Planken. "Incrementally Solving the STP by Enforcing Partial Path
  Consistency". PlanSIG 2008.
-}
ifpc :: (Foldable f, Ord v, Ord w, Num w)
  => NewConstraint v w        -- ^ The new constraint to add.
  -> f v                      -- ^ A foldable holding graph vertices.
  -> M'.Map (v, v) w          -- ^ A APSP distance map between any two vertices.
  -> Maybe (M'.Map (v, v) w)  -- ^ A Maybe of a new APSP distance map.
ifpc (edge_ab, w'_ab) vs wm
  | w'_ab + w_ab < 0 = Nothing
  | w'_ab >= w_ab = return wm
  | otherwise = return $ weights loop2Result
  where w_ab = wm M'.! edge_ab
        context = IFPCContext
          { weights = M'.insert edge_ab w'_ab wm
          , verts = vs
          , checkSetJ = Set.empty
          , checkSetI = Set.empty
          }

        -- The result of the loops.
        loop1Result = ifpcLoop1 edge_ab vs context
        loop2Result = ifpcLoop2 edge_ab loop1Result


-- -----------------------------------------------------------------------------
-- Helper functions and types for ifpc.
-- -----------------------------------------------------------------------------

data IFPCContext vert dist vertContainer = IFPCContext
  { weights :: M'.Map (vert, vert) dist
  , verts :: vertContainer
  , checkSetJ :: Set.Set vert
  , checkSetI :: Set.Set vert
  } deriving (Show)


ifpcLoop1 :: (Foldable f, Ord v, Ord w, Num w) =>
  (v, v) -> f v -> IFPCContext v w (f v) ->  IFPCContext v w (f v)
ifpcLoop1 edge@(a, b) vs context
  | null vs = context
  | otherwise = foldr (ifpcLoop1Inner edge) context filteredVerts
  where filteredVerts = filter (\v -> (v /= a) && (v /= b)) $ foldr (:) [] vs


ifpcLoop1Inner :: (Ord v, Ord w, Num w) =>
   (v, v) -> v -> IFPCContext v w (f v) -> IFPCContext v w (f v)
ifpcLoop1Inner (a, b) k context =
      IFPCContext
        { weights = nW
        , verts = verts context
        , checkSetI = niSet
        , checkSetJ = njSet
        }
    where
          -- Helper functions
          weightOf e = weights context M'.! e
          weightUpdate cond edge val w =
            if cond
              then M'.insert edge val w
              else w

          -- Aliases
          w_kab = weightOf (k, a) + weightOf (a, b)
          w_abk = weightOf (a, b) + weightOf (b, k)
          cond1 = weightOf (k, b) > w_kab
          cond2 = weightOf (a, k) > w_abk

          -- Update the context.
          nW = weightUpdate cond2 (a, k) w_abk
               $ weightUpdate cond1 (k, b) w_kab (weights context)
          niSet =
            if cond1
              then k `Set.insert` checkSetI context
              else checkSetI context
          njSet =
            if cond2
              then k `Set.insert` checkSetJ context
              else checkSetJ context


ifpcLoop2 :: (Ord v, Ord w, Num w) =>
  (v, v) -> IFPCContext v w (f v) -> IFPCContext v w (f v)
ifpcLoop2 (a, _) context =
  foldr (ifpcLoop2Inner a) context ijGroup
  where ijGroup = [ (i, j)
                  | i <- Set.toList $ checkSetI context
                  , j <- Set.toList $ checkSetJ context
                  , i /= j
                  ]


ifpcLoop2Inner :: (Ord v, Ord w, Num w) =>
  v -> (v, v) -> IFPCContext v w (f v) -> IFPCContext v w (f v)
ifpcLoop2Inner a (i, j) context
  = update context
    where
      weightOf edge = weights context M'.! edge
      w_iaj = weightOf (i, a) + weightOf (a, j)
      newWeights
        = if weightOf (i, j) > w_iaj
            then M'.insert (i, j) w_iaj (weights context)
            else weights context
      update x = x { weights = newWeights }

-- -----------------------------------------------------------------------------
-- Floyd Warshall Function
-- -----------------------------------------------------------------------------

inf :: (Fractional a) => a
inf = 1.0 / 0.0

{-|
   Generate an all-pairs shortest-path mapping between any two nodes.

   `e` is the list of nodes to schedule.
   `w` is a function which returns the weights between any two events.
   >>> floydwarshall eventList weightFunction
-}
floydWarshall
  :: (Ord node, Ord dist, Fractional dist)
  => [node]
  -> ((node, node) -> Maybe dist)
  -> M'.Map (node, node) dist
floydWarshall e = floydWarshallRec eventGroups
 where
  eventGroups = [ (i, j, k) | k <- e, i <- e, j <- e ]
  floydWarshallRec
    :: (Ord node, Ord dist, Fractional dist)
    => [(node, node, node)]
    -> ((node, node) -> Maybe dist)
    -> M'.Map (node, node) dist
  floydWarshallRec [] _  = mempty
  floydWarshallRec ((i, j, k) : es) w'
    = M'.insert (i, j) distUpdated previousWeights
   where
    distUpdated = min (getDist i j) (getDist i k + getDist k j)
    getDist x y =
      fromMaybe (fromMaybe inf (w' (x, y))) (previousWeights M'.!? (x, y))
    previousWeights = floydWarshallRec es w'


