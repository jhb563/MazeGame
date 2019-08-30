module Serialization where

import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import qualified Data.Text
import qualified Data.Vector as V

import MazeParser (dumpMaze)
import Player (WorldFeatures(..), LocationFeatures(..), produceWorldFeatures)
import Types

vectorizeWorld'' :: World -> V.Vector Float
vectorizeWorld'' w = gridFeatures V.++ playerFeatures V.++ enemyFeatures V.++ drillFeatures
  where
    player = worldPlayer w
    enemies = worldEnemies w
    playerFeatures = V.fromList $ fromIntegral <$>
      [fst . playerLocation $ player, snd . playerLocation $ player, fromIntegral $ playerCurrentStunDelay player, fromIntegral $ playerDrillsRemaining player]
    drills = worldDrillPowerUpLocations w
    drillFeatures = V.fromList $ fromIntegral <$> if length drills == 0 then [-1, -1, -1, -1]
      else if length drills == 1 then [fst (head drills), snd (head drills), -1, -1]
        else [fst (head drills), snd (head drills), fst (drills !! 1), snd (drills !! 1)]
    enemy1 = worldEnemies w !! 0
    enemy2 = worldEnemies w !! 1
    enemyFeatures = V.fromList $ fromIntegral <$>
      [ fst . enemyLocation $ enemy1, snd . enemyLocation $ enemy1, fromIntegral $ enemyCurrentStunTimer enemy1
      , fst . enemyLocation $ enemy2, snd . enemyLocation $ enemy2, fromIntegral $ enemyCurrentStunTimer enemy2
      ]
    gridFeatures = V.fromList $ worldGridNums w

vectorizeWorld' :: World -> V.Vector Float
vectorizeWorld' w = V.fromList (fromIntegral <$>
  [ lfOnActiveEnemy standStill
  , lfShortestPathLength standStill
  , lfManhattanDistance standStill
  , lfEnemiesOnPath standStill
  , lfNearestEnemyDistance standStill
  , lfNumNearbyEnemies standStill
  , lfStunAvailable standStill
  , lfDrillsRemaining standStill
  , lfMoveEase standStill
  , zeroIfNull (lfOnActiveEnemy <$> up)
  , zeroIfNull (lfShortestPathLength <$> up)
  , zeroIfNull (lfManhattanDistance <$> up)
  , zeroIfNull (lfEnemiesOnPath <$> up)
  , zeroIfNull (lfNearestEnemyDistance <$> up)
  , zeroIfNull (lfNumNearbyEnemies <$> up)
  , zeroIfNull (lfStunAvailable <$> up)
  , zeroIfNull (lfDrillsRemaining <$> up)
  , zeroIfNull (lfMoveEase <$> up)
  , zeroIfNull (lfOnActiveEnemy <$> right)
  , zeroIfNull (lfShortestPathLength <$> right)
  , zeroIfNull (lfManhattanDistance <$> right)
  , zeroIfNull (lfEnemiesOnPath <$> right)
  , zeroIfNull (lfNearestEnemyDistance <$> right)
  , zeroIfNull (lfNumNearbyEnemies <$> right)
  , zeroIfNull (lfStunAvailable <$> right)
  , zeroIfNull (lfDrillsRemaining <$> right)
  , zeroIfNull (lfMoveEase <$> right)
  , zeroIfNull (lfOnActiveEnemy <$> down)
  , zeroIfNull (lfShortestPathLength <$> down)
  , zeroIfNull (lfManhattanDistance <$> down)
  , zeroIfNull (lfEnemiesOnPath <$> down)
  , zeroIfNull (lfNearestEnemyDistance <$> down)
  , zeroIfNull (lfNumNearbyEnemies <$> down)
  , zeroIfNull (lfStunAvailable <$> down)
  , zeroIfNull (lfDrillsRemaining <$> down)
  , zeroIfNull (lfMoveEase <$> down)
  , zeroIfNull (lfOnActiveEnemy <$> left)
  , zeroIfNull (lfShortestPathLength <$> left)
  , zeroIfNull (lfManhattanDistance <$> left)
  , zeroIfNull (lfEnemiesOnPath <$> left)
  , zeroIfNull (lfNearestEnemyDistance <$> left)
  , zeroIfNull (lfNumNearbyEnemies <$> left)
  , zeroIfNull (lfStunAvailable <$> left)
  , zeroIfNull (lfDrillsRemaining <$> left)
  , zeroIfNull (lfMoveEase <$> left)
  ])
  where
    allFeatures = produceWorldFeatures w
    standStill = standStillFeatures allFeatures
    up = moveUpFeatures allFeatures
    right = moveRightFeatures allFeatures
    down = moveDownFeatures allFeatures
    left = moveLeftFeatures allFeatures

    zeroIfNull :: Maybe Int -> Int
    zeroIfNull = fromMaybe 0


vectorizeWorld :: World -> V.Vector Float
vectorizeWorld w = finalFeatures
  where
    initialGrid = V.fromList $ take 100 (repeat 0.0)
    (px, py) = playerLocation (worldPlayer w)
    (gx, gy) = endLocation w
    playerLocIndex = (9 - py) * 10 + px
    playerHasStun = playerCurrentStunDelay (worldPlayer w) == 0
    goalLocIndex = (9 - gy) * 10 + gx
    finalFeatures = initialGrid V.//
      ([(playerLocIndex, if playerHasStun then 25.0 else 10.0), (goalLocIndex, 100.0)] ++ enemyLocationUpdates)

    enemyLocationUpdates = enemyPair <$> (worldEnemies w)

    enemyPair e =
      let (ex, ey) = enemyLocation e
      in  ((9 - ey) * 10 + ex, if enemyCurrentStunTimer e == 0 then -25.0 else -10.0)

moveFromOutput :: V.Vector Float -> PlayerMove
moveFromOutput vals = PlayerMove moveDirection useStun moveDirection
  where
    bestMoveIndex = V.maxIndex vals
    moveDirection = case bestMoveIndex `mod` 5 of
      0 -> DirectionUp
      1 -> DirectionRight
      2 -> DirectionDown
      3 -> DirectionLeft
      4 -> DirectionNone
    useStun = bestMoveIndex > 4
