module Main where

import CuthillMcKee
import Graph
import Metrics
import Options.Applicative

newtype Options = Options
  { optInput :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
        ( long "input"
       <> metavar "FILE"
       <> help "Path to input graph file"
        )

main :: IO ()
main = do
  Options inputPath <- execParser opts
  graph <- readGraph inputPath
  let cm = cmOrdering graph
      icm = icmOrdering graph
      rcm = rcmOrdering graph
      bwCm = bandwidth (edges graph) cm
      bwIcm = bandwidth (edges graph) icm
      bwRcm = bandwidth (edges graph) rcm
  putStrLn $ "CM: " ++ unwords (map show cm)
  putStrLn $ "ICM: " ++ unwords (map show icm)
  putStrLn $ "RCM: " ++ unwords (map show rcm)
  putStrLn $ "BW_CM: " ++ show bwCm
  putStrLn $ "BW_ICM: " ++ show bwIcm
  putStrLn $ "BW_RCM: " ++ show bwRcm
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Cuthill-McKee graph ordering"
      )
