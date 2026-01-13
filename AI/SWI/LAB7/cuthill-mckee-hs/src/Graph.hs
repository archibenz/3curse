module Graph
  ( Graph
  , buildGraph
  , readGraph
  , vertexCount
  , neighbors
  , degree
  , edges
  , degreesMap
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Set as Set
import Data.Set (Set)


data Graph = Graph
  { graphN :: Int
  , adjacency :: IntMap IntSet
  , edgeList :: [(Int, Int)]
  } deriving (Show)

vertexCount :: Graph -> Int
vertexCount = graphN

neighbors :: Graph -> Int -> [Int]
neighbors graph v = IntSet.toList $ IntMap.findWithDefault IntSet.empty v (adjacency graph)

degree :: Graph -> Int -> Int
degree graph v = IntSet.size $ IntMap.findWithDefault IntSet.empty v (adjacency graph)

degreesMap :: Graph -> IntMap Int
degreesMap graph = IntMap.fromList [(v, degree graph v) | v <- [1 .. graphN graph]]

edges :: Graph -> [(Int, Int)]
edges = edgeList

buildGraph :: Int -> [(Int, Int)] -> Graph
buildGraph n rawEdges =
  let (adj, es) = foldl insertEdge (IntMap.fromList [(v, IntSet.empty) | v <- [1 .. n]], Set.empty) rawEdges
      edgeList' = Set.toList es
  in Graph n adj edgeList'
  where
    normalize (u, v)
      | u <= v = (u, v)
      | otherwise = (v, u)
    insertEdge (adjMap, edgeSet) (u, v)
      | u == v = (adjMap, edgeSet)
      | u < 1 || v < 1 || u > n || v > n = (adjMap, edgeSet)
      | otherwise =
          let pair = normalize (u, v)
          in if Set.member pair edgeSet
              then (adjMap, edgeSet)
              else (addAdj (addAdj adjMap u v) v u, Set.insert pair edgeSet)
    addAdj m from to = IntMap.insertWith IntSet.union from (IntSet.singleton to) m

readGraph :: FilePath -> IO Graph
readGraph path = do
  contents <- readFile path
  let ls = filter (not . null) (lines contents)
  case ls of
    [] -> error "Empty input file"
    (header:rest) ->
      case map read (words header) of
        [n, _m] ->
          let parsedEdges = map parseEdge rest
          in pure $ buildGraph n parsedEdges
        _ -> error "Invalid header format"
  where
    parseEdge line =
      case map read (words line) of
        [u, v] -> (u, v)
        _ -> error "Invalid edge line"
