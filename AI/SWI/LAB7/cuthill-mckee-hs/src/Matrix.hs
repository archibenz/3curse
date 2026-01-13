module Matrix
  ( MatrixStruct
  , InputFormat(..)
  , buildMatrix
  , readMatrix
  , matrixSize
  , structurePairs
  , adjacency
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Set as Set
import Data.Set (Set)


data InputFormat = FormatCoo | FormatDense deriving (Eq, Show)

newtype MatrixStruct = MatrixStruct
  { unMatrix :: MatrixData
  } deriving (Show)

data MatrixData = MatrixData
  { sizeN :: Int
  , pairs :: [(Int, Int)]
  , adj :: IntMap IntSet
  } deriving (Show)

matrixSize :: MatrixStruct -> Int
matrixSize = sizeN . unMatrix

structurePairs :: MatrixStruct -> [(Int, Int)]
structurePairs = pairs . unMatrix

adjacency :: MatrixStruct -> IntMap IntSet
adjacency = adj . unMatrix

buildMatrix :: Int -> [(Int, Int)] -> MatrixStruct
buildMatrix n coords =
  let undirected = foldl insertPair Set.empty coords
      adjMap = foldl addAdj (IntMap.fromList [(v, IntSet.empty) | v <- [1 .. n]]) (Set.toList undirected)
  in MatrixStruct (MatrixData n (Set.toList undirected) adjMap)
  where
    insertPair acc (i, j)
      | i == j = acc
      | i < 1 || j < 1 || i > n || j > n = acc
      | i < j = Set.insert (i, j) acc
      | otherwise = Set.insert (j, i) acc
    addAdj m (i, j) =
      let m' = IntMap.insertWith IntSet.union i (IntSet.singleton j) m
      in IntMap.insertWith IntSet.union j (IntSet.singleton i) m'

readMatrix :: InputFormat -> FilePath -> IO MatrixStruct
readMatrix format path = do
  contents <- readFile path
  let ls = filter (not . null) (lines contents)
  case format of
    FormatCoo -> parseCoo ls
    FormatDense -> parseDense ls

parseCoo :: [String] -> IO MatrixStruct
parseCoo ls =
  case ls of
    [] -> error "Empty input file"
    (header:rest) ->
      case map read (words header) of
        [n, _nnz] ->
          let coords = foldl collect [] rest
          in pure $ buildMatrix n coords
        _ -> error "Invalid COO header"
  where
    collect acc line =
      case map read (words line) of
        [i, j, val] -> if (val :: Double) == 0 then acc else (i, j) : acc
        _ -> error "Invalid COO line"

parseDense :: [String] -> IO MatrixStruct
parseDense ls =
  case ls of
    [] -> error "Empty input file"
    (header:rest) ->
      case map read (words header) of
        [n] ->
          let (rows, _remaining) = splitAt n rest
          in if length rows /= n
              then error "Not enough rows for dense matrix"
              else do
                let coords = concat (zipWith (rowCoords n) [1 .. n] rows)
                pure $ buildMatrix n coords
        _ -> error "Invalid dense header"
  where
    rowCoords n rowIndex line =
      let values = map read (words line) :: [Double]
      in if length values /= n
          then error "Dense row has incorrect length"
          else [ (rowIndex, colIndex) | (colIndex, val) <- zip [1 .. n] values, val /= 0 ]
