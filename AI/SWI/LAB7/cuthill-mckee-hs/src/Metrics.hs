module Metrics
  ( bandwidth
  ) where

import qualified Data.IntMap.Strict as IntMap

bandwidth :: [(Int, Int)] -> [Int] -> Int
bandwidth edgeList order =
  let positions = IntMap.fromList (zip order [1 :: Int ..])
      edgeBandwidth (u, v) =
        case (IntMap.lookup u positions, IntMap.lookup v positions) of
          (Just pu, Just pv) -> abs (pu - pv)
          _ -> 0
  in maximum (0 : map edgeBandwidth edgeList)
