module HaskellTemporalLib.Internal.Graph.Mut.VectorGraph
  ( floydWarshall
  , ifpcMatrixIO
  , ifpc
  , vertToIdxMap
  ) where

import           Control.Monad                  ( forM_
                                                , liftM2
                                                , when
                                                )
import qualified Data.Foldable                 as F
import qualified Data.Map.Strict               as M'
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import           HaskellTemporalLib.Internal.Graph.Matrix
                                                ( (!@)
                                                , Matrix(..)
                                                , toList
                                                , writeAt
                                                , toMap
                                                )
import           System.IO.Unsafe

------------------------------------------------------------------------------
-- Helper types.
------------------------------------------------------------------------------
type NewConstraint v e = ((v, v), e)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

(..<) :: (Enum a) => a -> a -> [a]
x ..< y = [x .. pred y]
infixr 5 ..<

idxToNodeMap :: [a] -> M'.Map Int a
idxToNodeMap xs = M'.fromList (zip [0 ..] xs)

intListToNodes :: [((Int, Int), a)] -> M'.Map Int node -> [((node, node), a)]
intListToNodes []                 _ = []
intListToNodes (((s, t), v) : xs) m = maybe rest (: rest) el
 where
  sM   = m M'.!? s
  tM   = m M'.!? t
  -- Make a tuple from the source and target if they exist.
  el   = liftM2 (\src tar -> ((src, tar), v)) sM tM
  rest = intListToNodes xs m

-- | Write a single weight to the matrix.
writeWeight
  :: (Fractional a)
  => Matrix (VM.IOVector a) -- ^ Matrix to write to.
  -> ((n, n) -> Maybe a)    -- ^ Weight function to map node pairs to weights.
  -> ((Int, n), (Int, n))   -- ^ Nodes and their respective matrix indices.
  -> IO ()
writeWeight m wf ((ridx, r), (cidx, c)) = writeAt m (ridx, cidx) toWriteValue
 where
  toWriteValue = if ridx == cidx then 0.0 else fromMaybe inf (wf (r, c))

-- ---------------------------------------------------------------------------
-- Floyd Warshall Function
-- ---------------------------------------------------------------------------

inf :: (Fractional a) => a
inf = 1.0 / 0.0

-- |  Generate an all-pairs shortest-path mapping between any two nodes.
-- Complexity is \( O(n^3) \).
floydWarshall
  :: (Ord node, Ord dist, Fractional dist)
  => [node] -- ^ List of nodes.
  -> ((node, node) -> Maybe dist) -- ^ Distance mapping function.
  -> M'.Map (node, node) dist -- ^ Returned APSP between nodes
floydWarshall es wf = unsafePerformIO $ floydWarshallIO es wf

-- | IO implementation of floydWarshall
floydWarshallIO
  :: (Ord node, Ord dist, Fractional dist)
  => [node]
  -> ((node, node) -> Maybe dist)
  -> IO (M'.Map (node, node) dist)
floydWarshallIO es wf =
  let numN    = length es
      -- Range for each node index.
      nIdx    = [0 .. numN - 1]
      -- Map of index to node.
      nodeMap = idxToNodeMap es
      doubleZippedEvents =
        [ (rtup, ctup) | ctup <- zip [0 ..] es, rtup <- zip [0 ..] es ]
  in  do
        vec <- VM.new (numN * numN)
        let mat = Matrix { width = numN, height = numN, dat = vec }
        -- Fill with the default weights provided by wf
        forM_ doubleZippedEvents (writeWeight mat wf)
        -- Update every cell in the matrix to find APSP
        floydWarshallUpdate mat
                            [ (i, j, k) | k <- nIdx, i <- nIdx, j <- nIdx ]
        -- Freeze the vector and pack it back in.
        frozenVec <- V.freeze $ dat mat
        let frozenMat =
              Matrix { width = numN, height = numN, dat = frozenVec }
        return (M'.fromList (intListToNodes (toList frozenMat) nodeMap))

-- | Update a distance in the matrix.
floydWarshallUpdate
  :: (Ord a, Num a) => Matrix (VM.IOVector a) -> [(Int, Int, Int)] -> IO ()
floydWarshallUpdate _ []               = pure ()
floydWarshallUpdate m ((i, j, k) : xs) = do
  ij <- m !@ (i, j)
  ik <- m !@ (i, k)
  kj <- m !@ (k, j)
  writeAt m (i, j) $ min ij (ik + kj)
  floydWarshallUpdate m xs

------------------------------------------------------------------------------
-- IFPC Functions
------------------------------------------------------------------------------

newCheckSet :: Int -> IO (VM.IOVector Bool)
newCheckSet numNodes = VM.new numNodes >>= fillCheckSet

fillCheckSet :: VM.IOVector Bool -> IO (VM.IOVector Bool)
fillCheckSet v = do
  forM_ (0 ..< VM.length v) $ \idx -> do
    VM.write v idx False
  pure v

rangeOverCheckSet :: V.Vector Bool -> [Int]
rangeOverCheckSet v =
  foldr (\(idx, x) acc -> if x then idx : acc else acc) []
    $ zip (0 ..< V.length v) (V.toList v)

data IFPCContext = IFPCContext
  { srcNode :: Int
  , tarNode :: Int
  , setI    :: VM.IOVector Bool
  , setJ    :: VM.IOVector Bool
  }

vertToIdxMap :: (Foldable f, Ord v) => f v -> M'.Map v Int
vertToIdxMap verts = M'.fromList (zip (F.toList verts) [0 ..])

ifpc
  :: (Ord v, Ord w, Fractional w, Foldable f)
  => NewConstraint v w
  -> f v
  -> M'.Map (v, v) w
  -> Maybe (M'.Map (v, v) w)
ifpc ((src, tar), w_prime) verts wf =
  let
    idxToVert i = V.fromList (F.toList verts) V.! i
    numVerts                     = length verts
    -- Create a matrix, freeze it, then convert to a map.
    (isConsistent, newWeightMat) = unsafePerformIO $ do
      vec <- VM.new (numVerts * numVerts)
      let m = Matrix numVerts numVerts vec
          vertToIdx v = vertToIdxMap verts M'.! v
          idxNewConstraint = ((vertToIdx src, vertToIdx tar), w_prime)
      -- Fill the matrix with the weight function.
      forM_ [ (a, b) | a <- 0 ..< numVerts, b <- 0 ..< numVerts ]
        $ \(r, c) -> do
            let toWriteValue = if r == c
                  then 0.0
                  else fromMaybe inf $ wf M'.!? (idxToVert r, idxToVert c)
            writeAt m (r, c) toWriteValue
      isConsistent_ <- ifpcMatrixIO idxNewConstraint m
      frozenVec     <- V.freeze $ dat m
      pure (isConsistent_, Matrix numVerts numVerts frozenVec)
  in
    if isConsistent then Just (toMap newWeightMat idxToVert) else Nothing

-- | Incremental Full Path Consistency.
-- Takes in a new constraint, a Foldable of vertices, and an
-- all-pairs-shortest-path distance Map.
--
-- Returns a newly minimised distance map if the new graph can be minimised.
--
-- [Léon Planken. "Incrementally Solving the STP by Enforcing Partial Path
-- Consistency". PlanSIG 2008.](http://www.macs.hw.ac.uk/~ruth/plansig08/ukplansig09_submission_6.pdf)
ifpcMatrixIO
  :: (Ord w, Num w)
  => NewConstraint Int w
  -- ^ The new constraint to add.
  -> Matrix (VM.IOVector w)
  -- ^ An APSP distance map between any two nodes.
  -> IO Bool
  -- ^ Returns an @IO@ of @True@ if consistent, otherwise @False@.
ifpcMatrixIO (toUpdate@(src, tar), newWeight) m = do
  w_ba <- m !@ (tar, src)
  w_ab <- m !@ (src, tar)
  if newWeight < negate w_ba
    -- We've just created an inconsistent network. Just return failure.
    then pure False
    else if newWeight >= w_ab
      -- Our network is still the same, we don't need to update anything.
      then pure True
      -- We have to propagate everything. This takes a while.
      else apspIO >> pure True
 where
  nodeRange = 0 ..< width m
  apspIO    = do
    -- Update the new weight in m
    writeAt m toUpdate newWeight
    -- Build the check sets
    checkSetI <- newCheckSet $ max (width m) (height m)
    checkSetJ <- newCheckSet $ max (width m) (height m)
    -- This updates m for any edges modified by the new constraint.
    let context = IFPCContext { srcNode = src
                              , tarNode = tar
                              , setI    = checkSetI
                              , setJ    = checkSetJ
                              }
    ifcpLoop1 m [ n | n <- nodeRange, n /= src, n /= tar ] context
    -- We won't modify these anymore, so we can freeze them.
    frozenSetI <- V.freeze checkSetI
    frozenSetJ <- V.freeze checkSetJ
    -- This updates m with any remaining things in the check set.
    ifcpLoop2
      m
      [ (i, j)
      | i <- rangeOverCheckSet frozenSetI
      , j <- rangeOverCheckSet frozenSetJ
      , i /= j
      ]
      src

  -- Update the weights, and build the check sets.
ifcpLoop1
  :: (Ord w, Num w) => Matrix (VM.IOVector w) -> [Int] -> IFPCContext -> IO ()
ifcpLoop1 _ []       _       = pure ()
ifcpLoop1 m (k : ks) context = do
  let a         = srcNode context
      b         = tarNode context
      checkSetI = setI context
      checkSetJ = setJ context
  -- Get all the weights at the start.
  newWeight <- m !@ (a, b)
  w_ak      <- m !@ (a, k)
  w_ka      <- m !@ (k, a)
  w_bk      <- m !@ (b, k)
  w_kb      <- m !@ (k, b)
  -- Update any weghts, and update the check sets accordingly.
  when (w_kb > w_ka + newWeight) $ do
    writeAt m (k, b) (w_ka + newWeight)
    VM.write checkSetI k True
  when (w_ak > newWeight + w_bk) $ do
    writeAt m (a, k) (newWeight + w_bk)
    VM.write checkSetJ k True
  ifcpLoop1 m ks context


-- Update any weights from the checksets.
ifcpLoop2
  :: (Ord w, Num w) => Matrix (VM.IOVector w) -> [(Int, Int)] -> Int -> IO ()
ifcpLoop2 _ []                   _   = pure ()
ifcpLoop2 m ((i, j) : remaining) src = do
  -- Retrieve all the weights (technically these can be done in parallel).
  w_ia <- m !@ (i, src)
  w_aj <- m !@ (src, j)
  w_ij <- m !@ (i, j)
  when (w_ij > w_ia + w_aj) $ do
    writeAt m (i, j) (w_ia + w_aj)
  ifcpLoop2 m remaining src

