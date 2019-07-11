module Main where

import Criterion
import Criterion.Main (defaultMain)
import System.Directory (getCurrentDirectory)
import System.Random

import Analyzer
import WorldParser (loadWorldFromFile)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let mazeFile = currentDir ++ "/benchmark/maze_save_2"
  world <- loadWorldFromFile mazeFile
  let numIterations = 1
  defaultMain
    [ bgroup "World Running Test"
      [ bench "Analyze Enemies" $ whnf (runAllIterations 1 world) varyNumEnemies
      , bench "Analyze Drills" $ whnf (runAllIterations 1 world) varyNumDrillPickups
      , bench "Analzye Cooldown" $ whnf (runAllIterations 1 world) varyPlayerStunCooldown
      ]
    ]
