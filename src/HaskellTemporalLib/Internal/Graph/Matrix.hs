module HaskellTemporalLib.Internal.Graph.Matrix where

import qualified Data.Bifunctor                as Bif
import qualified Data.Map.Strict               as M'
import           Data.Maybe
import qualified Data.Set                      as Set
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM

data Matrix v = Matrix
  { width  :: Int
  , height :: Int
  , dat    :: v
  }
  deriving (Show, Eq)

at :: Matrix (V.Vector a) -> (Int, Int) -> a
at m edge = dat m V.! (m `idxAt` edge)

idxAt :: Matrix (V.Vector a) -> (Int, Int) -> Int
idxAt m (r, c) = r * width m + c

-- | Get a value at a specified (row, column).
atIO :: Matrix (VM.IOVector a) -> (Int, Int) -> IO a
atIO m (r, c) = dat m `VM.read` (r * width m + c)

-- | Infix alias of @atIO@
(!@) :: Matrix (VM.IOVector a) -> (Int, Int) -> IO a
m !@ e = atIO m e

writeAt :: Matrix (VM.IOVector a) -> (Int, Int) -> a -> IO ()
writeAt m (r, c) = VM.write (dat m) (r * width m + c)

-- | Convert a matrix to a list of row-column-distance values
toList :: Matrix (V.Vector dist) -> [((Int, Int), dist)]
toList m = [ ((r, c), m `at` (r, c)) | c <- wRange, r <- hRange ]
 where
  wRange = [0 .. width m - 1]
  hRange = [0 .. height m - 1]

-- | Convert an immutable to a mutable matrix safely.
thaw
  :: Matrix (V.Vector a)
  -> IO (Matrix (VM.IOVector a))
thaw mat = do
  newDat <- V.thaw $ dat mat
  pure mat { dat = newDat }

-- | Convert a mutable matrix to an immutable matrix.
freeze
  :: Matrix (VM.IOVector a)
  -> IO (Matrix (V.Vector a))
freeze mat = do
  newDat <- V.freeze $ dat mat
  pure mat { dat = newDat }

-- | Convert a Vector Matrix to an edge map.
toMap :: (Ord v) => Matrix (V.Vector a) -> (Int -> v) -> M'.Map (v, v) a
toMap m g = M'.fromList
  [ ((g r, g c), m `at` (r, c)) | c <- wRange, r <- hRange ]
 where
  wRange = [0 .. width m - 1]
  hRange = [0 .. height m - 1]

uniqueKeys :: (Ord a) => [(a, a)] -> [a]
uniqueKeys xs =
  let tupToList (i, j) = [i, j]
      expandedList = concatMap tupToList xs
  in  Set.toList $ Set.fromList expandedList

-- | Lets you define how non-existant distances are mapped to values.
--
-- Some common mapping functions are...
--
-- To mark "unreachable" as infinite weight:
-- >>> CellMapper inf id
--
-- To mark "unreachable" as Nothing:
-- >>> CellMapper Nothing Just
--
-- To mark unreachable as zero weight:
-- >>> CellMapper 0 id
data CellMapper a b = CellMapper b (a -> b)

-- | Convert a map of @(vertex, vertex)=>distance@ into a distance matrix.
--
-- If a distance is not labelled, its cell is the default cellType, specified
-- by the
fromMap
  :: M'.Map (v, v) d                    -- ^ Map to convert.
  -> (v -> Int)  -- ^ Function to convert a vertex to row or column index.
  -> CellMapper d cellType              -- ^ Default mapping function.
  -> Matrix (V.Vector cellType)         -- ^ Resulting matrix.
fromMap mp f (CellMapper def defFunc) =
  let
    maxNode   = maximum $ (\(i, j) -> max (f i) (f j)) <$> M'.keys mp
    idxMap    = M'.mapKeys (Bif.bimap f f) mp
    widthVal  = maxNode + 1
    heightVal = widthVal
    idxToRowCol idx = (idx `div` widthVal, idx `mod` widthVal)
    idxToWeight idx = fromMaybe
      def
      (Just . defFunc =<< (flip M'.lookup idxMap . idxToRowCol) idx)
  in
    Matrix { width  = widthVal
           , height = heightVal
           , dat    = V.generate (heightVal * widthVal) idxToWeight
           }
-- | Convert a map of @(vertex, vertex)=>distance@ into a distance matrix.
--
-- If a distance is not labelled, its cell is Nothing.
fromMapToMaybeMatrix
  :: M'.Map (v, v) d -- ^ Map to convert.
  -> (v -> Int)  -- ^ Function to convert a vertex to row or column index.
  -> Matrix (V.Vector (Maybe d)) -- ^ Resulting matrix.
fromMapToMaybeMatrix mp f = fromMap mp f $ CellMapper Nothing Just
