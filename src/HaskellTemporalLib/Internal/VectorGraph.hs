module HaskellTemporalLib.Internal.VectorGraph
  ( floydWarshall
  ) where

import           Control.Monad                  ( liftM2 )
import qualified Data.Map.Strict               as M'
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import           System.IO.Unsafe


data Matrix v = Matrix
  { width  :: Int
  , height :: Int
  , dat    :: v
  }
  deriving Show

at :: Matrix (V.Vector a) -> (Int, Int) -> a
at m (r, c) = dat m V.! (r * width m + c)

atIO :: Matrix (VM.IOVector a) -> (Int, Int) -> IO a
atIO m (r, c) = dat m `VM.read` (r * width m + c)

writeAt :: Matrix (VM.IOVector a) -> (Int, Int) -> a -> IO ()
writeAt m (r, c) = VM.write (dat m) (r * width m + c)

matrixToList :: Matrix (V.Vector dist) -> [((Int, Int), dist)]
matrixToList m =
  [ ((r, c), m `at` (r, c))
  | c <- [0 .. width m - 1]
  , r <- [0 .. height m - 1]
  ]

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
writeWeight m wf ((ridx, r), (cidx, c)) =
  writeAt m (ridx, cidx) (fromMaybe inf (wf (r, c)))

-- ---------------------------------------------------------------------------
-- Floyd Warshall Function
-- ---------------------------------------------------------------------------

inf :: (Fractional a) => a
inf = 1.0 / 0.0

-- |  Generate an all-pairs shortest-path mapping between any two nodes.
-- Complexity is \( O(n^3) \).
floydWarshall
  :: (Ord node, Ord dist, Fractional dist)
  => [node]
  -> ((node, node) -> Maybe dist)
  -> M'.Map (node, node) dist
floydWarshall es wf = unsafePerformIO $ floydWarshallIO es wf

-- | IO implementation of floydWarshall
floydWarshallIO
  :: (Ord node, Ord dist, Fractional dist)
  => [node]
  -> ((node, node) -> Maybe dist)
  -> IO (M'.Map (node, node) dist)
floydWarshallIO es wf =
  let numNodes = length es
      -- Range for each node index.
      nIdx     = [0 .. numNodes - 1]
      -- Map of index to node.
      nodeMap  = idxToNodeMap es
      doubleZippedEvents =
        [ (rtup, ctup) | ctup <- zip [0 ..] es, rtup <- zip [0 ..] es ]
  in  do
        vec <- VM.new (numNodes * numNodes)
        let mat = Matrix { width = numNodes, height = numNodes, dat = vec }
         -- Fill with the default weights provided by wf
        mapM_ (writeWeight mat wf) doubleZippedEvents
        -- Update every cell in the matrix to find APSP
        updateDist mat [ (i, j, k) | k <- nIdx, i <- nIdx, j <- nIdx ]
        -- Freeze the vector and pack it back in.
        frozenVec <- V.freeze $ dat mat
        let frozenMat = Matrix { width  = numNodes
                               , height = numNodes
                               , dat    = frozenVec
                               }
        return (M'.fromList (intListToNodes (matrixToList frozenMat) nodeMap))

-- | Update a distance in the matrix.
updateDist
  :: (Ord a, Num a) => Matrix (VM.IOVector a) -> [(Int, Int, Int)] -> IO ()
updateDist _ []               = return ()
updateDist m ((i, j, k) : xs) = do
  ij <- m `atIO` (i, j)
  ik <- m `atIO` (i, k)
  kj <- m `atIO` (k, j)
  writeAt m (i, j) $ min ij (ik + kj)
  updateDist m xs
