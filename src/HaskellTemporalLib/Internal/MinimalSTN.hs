module HaskellTemporalLib.Internal.MinimalSTN
  ( minimise
  , generalise
  )
where


import HaskellTemporalLib.Internal.Stn (SimpleTemporalNetwork(..))
import HaskellTemporalLib.Internal.Graph.Mut.VectorGraph (floydWarshall)

-- | Wrapper type to indicate minimality at compile time.
-- With a minimal STN, you can conduct iterative minimisations on the
-- contained data.
newtype MinimalSTN n event weight
  = MinimalSTN (n event weight) deriving (Show, Eq)


instance (SimpleTemporalNetwork n) => SimpleTemporalNetwork (MinimalSTN n) where
  zEvent (MinimalSTN net) = zEvent net
  events (MinimalSTN net) = events net
  cnst fr to (MinimalSTN net) = cnst fr to net
  fromList z xs = MinimalSTN (fromList z xs)
  fromMap z m = MinimalSTN (fromMap z m)
  toMap (MinimalSTN net) = toMap net


-- | Remove minimisation guarantees and return the wrapped STN.
--
-- The underlying data remains unchanged, but you will no longer be able to
-- compute metrics which require minimisation on the returned value.
generalise :: MinimalSTN n event weight -> n event weight
generalise (MinimalSTN x) = x


-- | Minimise a Simple Temporal Network
-- Conducts an All-Pairs-Shortest-Path minimisation on the network graph.
-- Complexity dependent on specific implementation.
--
-- Returns a Maybe MinimalSTN wrapper type around the passed in
-- SimpleTemporalNetwork type.
minimise
  :: (SimpleTemporalNetwork net, Ord event, Ord weight, Fractional weight)
  => net event weight
  -- ^ The Simple Temporal Network to minimise
  -> Maybe (MinimalSTN net event weight)
  -- ^ Newly minimised SimpleTemporalNetwork
minimise stn
  = if isConsistent newStn
      then Just (MinimalSTN newStn)
      else Nothing
  where
    newMap = floydWarshall (events stn) (\(x, y) -> cnst x y stn)
    newStn = fromMap (zEvent stn) newMap
