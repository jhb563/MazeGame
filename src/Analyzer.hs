module Analyzer where

import Control.Monad.State
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Random (mkStdGen, StdGen)

import Runner (updateFunc, generateRandomLocation, mkNewEnemy)
import WorldParser (loadWorldFromFile)
import Types

runAnalysis :: (World -> Float) -> IO ()
runAnalysis evaluationFunction = do
  args <- getArgs
  currentDir <- getCurrentDirectory
  let filepath = currentDir ++ "/" ++ (head args)
  if "--enemies" `elem` args
    then analyzeNumEnemies evaluationFunction filepath
    else if "--drills" `elem` args
      then analyzeNumDrillPickups evaluationFunction filepath
      else analyzeStunCooldown evaluationFunction filepath

analyzeNumEnemies :: (World -> Float) -> FilePath -> IO ()
analyzeNumEnemies evaluationFunction fp = do
  world <- loadWorldFromFile fp
  let numIterations = 10
  putStrLn "Analyzing Different Numbers of Enemies"
  let results = runAllIterations numIterations world evaluationFunction varyNumEnemies
  forM_ results $ \(gp, numWins) -> putStrLn $
    "With " ++ (show (numEnemies gp)) ++ " Enemies: " ++ (show numWins)
      ++ " wins out of " ++ (show numIterations) ++ " iterations."

analyzeNumDrillPickups :: (World -> Float) -> FilePath -> IO ()
analyzeNumDrillPickups evaluationFunction fp = do
  world <- loadWorldFromFile fp
  let numIterations = 10
  putStrLn "Analyzing Different Numbers of Drill Pickups"
  let results = runAllIterations numIterations world evaluationFunction varyNumDrillPickups
  forM_ results $ \(gp, numWins) -> putStrLn $
    "With " ++ (show (numDrillPowerups gp)) ++ " Drills: " ++ (show numWins)
      ++ " wins out of " ++ (show numIterations) ++ " iterations."

analyzeStunCooldown :: (World -> Float) -> FilePath -> IO ()
analyzeStunCooldown evaluationFunction fp = do
  world <- loadWorldFromFile fp
  let numIterations = 100
  putStrLn "Analyzing Different Stun Cooldown Times"
  let results = runAllIterations numIterations world evaluationFunction varyPlayerStunCooldown
  forM_ results $ \(gp, numWins) -> putStrLn $
    "With " ++ (show (initialStunTimer . playerGameParameters $ gp)) ++ " Initial Cooldown: " ++ (show numWins)
      ++ " wins out of " ++ (show numIterations) ++ " iterations."

runAllIterations :: Int -> World -> (World -> Float) -> (GameParameters -> [GameParameters]) -> [(GameParameters, Int)]
runAllIterations numIterations w evaluationFunction paramGenerator = map countWins results
  where
    aiParams = (worldParameters w) { usePlayerAI = True }
    paramSets = paramGenerator aiParams

    runParamSet :: GameParameters -> [GameResult]
    runParamSet ps = map (runGame w {worldParameters = ps }) [1..numIterations]

    runGame :: World -> Int -> GameResult
    runGame baseWorld seed = runGameToResult evaluationFunction (generateWorldIteration baseWorld (mkStdGen seed))

    results :: [(GameParameters, [GameResult])]
    results = zip paramSets (map runParamSet paramSets)

    countWins :: (GameParameters, [GameResult]) -> (GameParameters, Int)
    countWins (gp, gameResults) = (gp, length (filter (== GameWon) gameResults))

varyNumEnemies :: GameParameters -> [GameParameters]
varyNumEnemies baseParams = newParams <$> allEnemyNumbers
  where
    baseNumEnemies = numEnemies baseParams
    allEnemyNumbers = [baseNumEnemies..(baseNumEnemies + 9)]
    newParams i = baseParams { numEnemies = i }

varyNumDrillPickups :: GameParameters -> [GameParameters]
varyNumDrillPickups baseParams = newParams <$> allDrillNumbers
  where
    baseNumDrills = numDrillPowerups baseParams
    allDrillNumbers = [baseNumDrills..(baseNumDrills + 9)]
    newParams i = baseParams { numDrillPowerups = i }

varyPlayerStunCooldown :: GameParameters -> [GameParameters]
varyPlayerStunCooldown baseParams = newParams <$> allCooldowns
  where
    basePlayerParams = playerGameParameters baseParams
    baseCooldown = initialStunTimer basePlayerParams
    allCooldowns = [(baseCooldown - 4)..(baseCooldown + 5)]
    newParams i = baseParams
      { playerGameParameters = basePlayerParams { initialStunTimer = i }}

runGameToResult :: (World -> Float) -> World -> GameResult
runGameToResult evaluationFunction = evalState runGameState
  where
    runGameState :: State World GameResult
    runGameState = do
      modify (updateFunc evaluationFunction 1.0)
      currentResult <- gets worldResult
      if currentResult /= GameInProgress
        then return currentResult
        else runGameState

generateWorldIteration :: World -> StdGen -> World
generateWorldIteration w gen1 = w
  { worldEnemies = enemies
  , worldDrillPowerUpLocations = drillLocations
  , worldRandomGenerator = gen3
  , worldTime = 0
  }
  where
    params = worldParameters w
    rowCount = numRows params
    columnCount = numColumns params
    enemyCount = numEnemies params
    drillCount = numDrillPowerups params

    (enemyLocations, gen2) = runState
      (sequence (map (const (generateRandomLocation (rowCount, columnCount))) [1..enemyCount]))
      gen1
    (drillLocations, gen3) = runState
      (sequence (map (const (generateRandomLocation (rowCount, columnCount))) [1..drillCount]))
      gen2
    enemies = mkNewEnemy (enemyGameParameters params) <$> enemyLocations
