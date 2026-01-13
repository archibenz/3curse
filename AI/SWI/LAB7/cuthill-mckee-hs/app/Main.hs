module Main where

import CuthillMcKee
import Matrix
import Metrics
import Options.Applicative
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Control.Applicative ((<|>))


data Algorithm = AlgCm | AlgIcm | AlgRcm | AlgAll deriving (Eq, Show)

data Options = Options
  { optInput :: FilePath
  , optFormat :: InputFormat
  , optAlgorithm :: Algorithm
  , optShowMatrix :: Bool
  , optShowPositions :: Bool
  , optOneBased :: Bool
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
        ( long "input"
       <> metavar "FILE"
       <> help "Path to input matrix file"
        )
  <*> option formatReader
        ( long "format"
       <> metavar "coo|dense"
       <> help "Input format"
        )
  <*> option algorithmReader
        ( long "algorithm"
       <> metavar "cm|icm|rcm|all"
       <> value AlgAll
       <> help "Ordering algorithm (default: all)"
        )
  <*> switch
        ( long "show-matrix-before-after"
       <> help "Print structural nonzeros before/after ordering"
        )
  <*> switch
        ( long "show-positions"
       <> help "Print position table p(i) = pos(i)"
        )
  <*> ( flag' True
        ( long "1based"
       <> help "Output 1-based indices (default)"
        )
     <|> flag True False
        ( long "0based"
       <> help "Output 0-based indices"
        )
      )

formatReader :: ReadM InputFormat
formatReader = eitherReader $ \value ->
  case value of
    "coo" -> Right FormatCoo
    "dense" -> Right FormatDense
    _ -> Left "Format must be coo or dense"

algorithmReader :: ReadM Algorithm
algorithmReader = eitherReader $ \value ->
  case value of
    "cm" -> Right AlgCm
    "icm" -> Right AlgIcm
    "rcm" -> Right AlgRcm
    "all" -> Right AlgAll
    _ -> Left "Algorithm must be cm, icm, rcm, or all"

main :: IO ()
main = do
  Options inputPath format algorithm showMatrix showPositions oneBased <- execParser opts
  matrix <- readMatrix format inputPath
  let adj = adjacency matrix
      pairs = structurePairs matrix
      n = matrixSize matrix
      cm = cmOrder adj
      icm = icmOrder adj
      rcm = rcmOrder adj
      orders = case algorithm of
        AlgCm -> [("CM", cm)]
        AlgIcm -> [("ICM", icm)]
        AlgRcm -> [("RCM", rcm)]
        AlgAll -> [("CM", cm), ("ICM", icm), ("RCM", rcm)]
  if showMatrix
    then do
      putStrLn "STRUCTURE_BEFORE:"
      mapM_ putStrLn (formatPairs oneBased pairs)
    else pure ()
  mapM_ (printOrder n pairs oneBased showPositions showMatrix) orders
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Cuthill-McKee ordering for sparse matrix structure"
      )

printOrder :: Int -> [(Int, Int)] -> Bool -> Bool -> Bool -> (String, [Int]) -> IO ()
printOrder n pairs oneBased showPositions showMatrix (label, order) = do
  putStrLn $ label ++ ": " ++ unwords (map (show . formatIndex oneBased) order)
  putStrLn $ "BW_" ++ label ++ ": " ++ show (bandwidthFromStructure n pairs order)
  if showPositions
    then do
      putStrLn $ "P_" ++ label ++ ":"
      mapM_ putStrLn (formatPositions oneBased n order)
    else pure ()
  if showMatrix
    then do
      putStrLn $ "STRUCTURE_AFTER_" ++ label ++ ":"
      mapM_ putStrLn (formatPairs oneBased (reorderPairs order pairs))
    else pure ()

formatIndex :: Bool -> Int -> Int
formatIndex oneBased v = if oneBased then v else v - 1

formatPositions :: Bool -> Int -> [Int] -> [String]
formatPositions oneBased n order =
  let positions = IntMap.fromList (zip order [1 :: Int ..])
  in [ show (formatIndex oneBased v) ++ " " ++ show (formatIndex oneBased pos)
     | v <- [1 .. n]
     , let pos = IntMap.findWithDefault 0 v positions
     ]

formatPairs :: Bool -> [(Int, Int)] -> [String]
formatPairs oneBased coords =
  [ show (formatIndex oneBased i) ++ " " ++ show (formatIndex oneBased j)
  | (i, j) <- coords
  ]

reorderPairs :: [Int] -> [(Int, Int)] -> [(Int, Int)]
reorderPairs order coords =
  let positions = IntMap.fromList (zip order [1 :: Int ..])
      normalized = [ normalize (pos i) (pos j) | (i, j) <- coords ]
  in Set.toList (Set.fromList normalized)
  where
    pos v = IntMap.findWithDefault 0 v positions
    normalize a b = if a <= b then (a, b) else (b, a)
