module CuthillMcKee
  ( cmOrdering
  , icmOrdering
  , rcmOrdering
  ) where

import Graph
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))
import Data.List (sortBy)
import Data.Ord (comparing)

cmOrdering :: Graph -> [Int]
cmOrdering graph =
  let degs = degreesMap graph
  in coverComponents graph degs selectMinDegree

icmOrdering :: Graph -> [Int]
icmOrdering graph =
  let degs = degreesMap graph
  in coverComponents graph degs selectPseudoPeripheral

rcmOrdering :: Graph -> [Int]
rcmOrdering graph = reverse (icmOrdering graph)

coverComponents :: Graph -> IntMap Int -> (Graph -> IntMap Int -> IntSet -> Int -> Int) -> [Int]
coverComponents graph degs chooseStart = go IntSet.empty []
  where
    allVertices = IntSet.fromList [1 .. vertexCount graph]
    go visited acc
      | visited == allVertices = reverse acc
      | otherwise =
          let unvisited = IntSet.difference allVertices visited
              baseStart = selectMinDegreeVertex degs unvisited
              start = chooseStart graph degs unvisited baseStart
              (order, visited') = bfsOrder graph degs visited start
          in go visited' (reverse order ++ acc)

selectMinDegree :: Graph -> IntMap Int -> IntSet -> Int -> Int
selectMinDegree _ _ _ start = start

selectPseudoPeripheral :: Graph -> IntMap Int -> IntSet -> Int -> Int
selectPseudoPeripheral graph degs unvisited start =
  let component = componentVertices graph unvisited start
  in pseudoPeripheral graph degs component start

bfsOrder :: Graph -> IntMap Int -> IntSet -> Int -> ([Int], IntSet)
bfsOrder graph degs visited start = go (Seq.singleton start) (IntSet.insert start visited) []
  where
    go queue seen acc =
      case Seq.viewl queue of
        Seq.EmptyL -> (reverse acc, seen)
        v Seq.:< rest ->
          let sortedNeighbors = sortedUnvisitedNeighbors graph degs seen v
              seen' = foldl (flip IntSet.insert) seen sortedNeighbors
              queue' = rest Seq.>< Seq.fromList sortedNeighbors
          in go queue' seen' (v : acc)

sortedUnvisitedNeighbors :: Graph -> IntMap Int -> IntSet -> Int -> [Int]
sortedUnvisitedNeighbors graph degs visited v =
  let candidates = filter (`IntSet.notMember` visited) (neighbors graph v)
  in sortBy (comparing (neighborKey degs)) candidates

neighborKey :: IntMap Int -> Int -> (Int, Int)
neighborKey degs v = (IntMap.findWithDefault 0 v degs, v)

selectMinDegreeVertex :: IntMap Int -> IntSet -> Int
selectMinDegreeVertex degs vertices =
  IntSet.foldl' pick (IntSet.findMin vertices) vertices
  where
    pick best v =
      let (degBest, degV) = (IntMap.findWithDefault 0 best degs, IntMap.findWithDefault 0 v degs)
      in if degV < degBest || (degV == degBest && v < best)
            then v
            else best

componentVertices :: Graph -> IntSet -> Int -> IntSet
componentVertices graph allowed start = go (Seq.singleton start) (IntSet.singleton start)
  where
    go queue seen =
      case Seq.viewl queue of
        Seq.EmptyL -> seen
        v Seq.:< rest ->
          let next = filter (`IntSet.member` allowed) (neighbors graph v)
              unseen = filter (`IntSet.notMember` seen) next
              seen' = foldl (flip IntSet.insert) seen unseen
              queue' = rest Seq.>< Seq.fromList unseen
          in go queue' seen'

bfsLevels :: Graph -> IntSet -> Int -> [[Int]]
bfsLevels graph allowed start = go [start] (IntSet.singleton start) []
  where
    go [] _ acc = reverse acc
    go current seen acc =
      let neighborsList = concatMap (neighbors graph) current
          nextCandidates = filter (`IntSet.member` allowed) neighborsList
          nextLevelSet = IntSet.fromList (filter (`IntSet.notMember` seen) nextCandidates)
          nextLevel = IntSet.toList nextLevelSet
          seen' = IntSet.union seen nextLevelSet
      in go nextLevel seen' (current : acc)

pseudoPeripheral :: Graph -> IntMap Int -> IntSet -> Int -> Int
pseudoPeripheral graph degs component start = go start (-1)
  where
    go current prevEcc =
      let levels = bfsLevels graph component current
          ecc = length levels - 1
          lastLevel = case reverse levels of
            [] -> [current]
            (lvl:_) -> lvl
          next = minDegreeInList degs lastLevel
      in if ecc > prevEcc
          then go next ecc
          else current

minDegreeInList :: IntMap Int -> [Int] -> Int
minDegreeInList degs (x:xs) = foldl pick x xs
  where
    pick best v =
      let (degBest, degV) = (IntMap.findWithDefault 0 best degs, IntMap.findWithDefault 0 v degs)
      in if degV < degBest || (degV == degBest && v < best)
            then v
            else best
minDegreeInList _ [] = error "Empty vertex list"
