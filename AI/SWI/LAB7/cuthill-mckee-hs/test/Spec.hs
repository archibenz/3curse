module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as Set
import CuthillMcKee
import Graph
import Metrics

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Cuthill-McKee"
  [ testCase "orders are permutations" testPermutations
  , testCase "RCM is reverse of ICM" testRcmReverse
  , testCase "bandwidth values" testBandwidth
  ]

sampleGraph :: Graph
sampleGraph = buildGraph 10
  [ (1,6)
  , (1,8)
  , (2,5)
  , (3,4)
  , (3,8)
  , (5,7)
  , (5,10)
  , (7,8)
  , (9,10)
  ]

assertPermutation :: Int -> [Int] -> Assertion
assertPermutation n order = do
  length order @?= n
  Set.size (Set.fromList order) @?= n


testPermutations :: Assertion
testPermutations = do
  let n = vertexCount sampleGraph
      cm = cmOrdering sampleGraph
      icm = icmOrdering sampleGraph
      rcm = rcmOrdering sampleGraph
  assertPermutation n cm
  assertPermutation n icm
  assertPermutation n rcm


testRcmReverse :: Assertion
testRcmReverse = do
  let icm = icmOrdering sampleGraph
      rcm = rcmOrdering sampleGraph
  rcm @?= reverse icm


testBandwidth :: Assertion
testBandwidth = do
  let cm = cmOrdering sampleGraph
      icm = icmOrdering sampleGraph
      rcm = rcmOrdering sampleGraph
      bwCm = bandwidth (edges sampleGraph) cm
      bwIcm = bandwidth (edges sampleGraph) icm
      bwRcm = bandwidth (edges sampleGraph) rcm
  bwCm @?= 3
  bwIcm @?= 2
  bwRcm @?= 2
