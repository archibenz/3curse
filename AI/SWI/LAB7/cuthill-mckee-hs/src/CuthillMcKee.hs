module CuthillMcKee
  ( cmOrder
  , icmOrder
  , rcmOrder
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))
import Data.List (sortBy)
import Data.Ord (comparing)


type Adj = IntMap IntSet

cmOrder :: Adj -> [Int]
cmOrder adj =
  let degs = degreesMap adj
      n = IntMap.size adj
  in coverComponents adj degs n selectMinDegree

icmOrder :: Adj -> [Int]
icmOrder adj =
  let degs = degreesMap adj
      n = IntMap.size adj
  in coverComponents adj degs n selectPseudoPeripheral

rcmOrder :: Adj -> [Int]
rcmOrder adj = reverse (icmOrder adj)

coverComponents :: Adj -> IntMap Int -> Int -> (Adj -> IntMap Int -> IntSet -> Int -> Int) -> [Int]
coverComponents adj degs n chooseStart = go IntSet.empty []
  where
    allVertices = IntSet.fromList [1 .. n]
    go visited acc
      | visited == allVertices = reverse acc
      | otherwise =
          let unvisited = IntSet.difference allVertices visited
              baseStart = selectMinDegreeVertex degs unvisited
              start = chooseStart adj degs unvisited baseStart
              (order, visited') = bfsOrder adj degs visited start
          in go visited' (reverse order ++ acc)

selectMinDegree :: Adj -> IntMap Int -> IntSet -> Int -> Int
selectMinDegree _ _ _ start = start

selectPseudoPeripheral :: Adj -> IntMap Int -> IntSet -> Int -> Int
selectPseudoPeripheral adj degs unvisited start =
  let component = componentVertices adj unvisited start
  in pseudoPeripheral adj degs component start

bfsOrder :: Adj -> IntMap Int -> IntSet -> Int -> ([Int], IntSet)
bfsOrder adj degs visited start = go (Seq.singleton start) (IntSet.insert start visited) []
  where
    go queue seen acc =
      case Seq.viewl queue of
        Seq.EmptyL -> (reverse acc, seen)
        v Seq.:< rest ->
          let sortedNeighbors = sortedUnvisitedNeighbors adj degs seen v
              seen' = foldl (flip IntSet.insert) seen sortedNeighbors
              queue' = rest Seq.>< Seq.fromList sortedNeighbors
          in go queue' seen' (v : acc)

sortedUnvisitedNeighbors :: Adj -> IntMap Int -> IntSet -> Int -> [Int]
sortedUnvisitedNeighbors adj degs visited v =
  let candidates = filter (`IntSet.notMember` visited) (neighbors adj v)
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

componentVertices :: Adj -> IntSet -> Int -> IntSet
componentVertices adj allowed start = go (Seq.singleton start) (IntSet.singleton start)
  where
    go queue seen =
      case Seq.viewl queue of
        Seq.EmptyL -> seen
        v Seq.:< rest ->
          let next = filter (`IntSet.member` allowed) (neighbors adj v)
              unseen = filter (`IntSet.notMember` seen) next
              seen' = foldl (flip IntSet.insert) seen unseen
              queue' = rest Seq.>< Seq.fromList unseen
          in go queue' seen'

bfsLevels :: Adj -> IntSet -> Int -> [[Int]]
bfsLevels adj allowed start = go [start] (IntSet.singleton start) []
  where
    go [] _ acc = reverse acc
    go current seen acc =
      let neighborsList = concatMap (neighbors adj) current
          nextCandidates = filter (`IntSet.member` allowed) neighborsList
          nextLevelSet = IntSet.fromList (filter (`IntSet.notMember` seen) nextCandidates)
          nextLevel = IntSet.toList nextLevelSet
          seen' = IntSet.union seen nextLevelSet
      in go nextLevel seen' (current : acc)

pseudoPeripheral :: Adj -> IntMap Int -> IntSet -> Int -> Int
pseudoPeripheral adj degs component start = go start (-1)
  where
    go current prevEcc =
      let levels = bfsLevels adj component current
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

neighbors :: Adj -> Int -> [Int]
neighbors adj v = IntSet.toList $ IntMap.findWithDefault IntSet.empty v adj

degreesMap :: Adj -> IntMap Int
degreesMap adj = IntMap.map IntSet.size adj
