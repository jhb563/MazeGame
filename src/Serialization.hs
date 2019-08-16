module Serialization where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Player (WorldFeatures(..), LocationFeatures(..), produceWorldFeatures)
import Types

vectorizeWorld :: World -> V.Vector Float
vectorizeWorld w = V.fromList (fromIntegral <$>
  [ lfOnActiveEnemy standStill
  , lfShortestPathLength standStill
  , lfManhattanDistance standStill
  , lfEnemiesOnPath standStill
  , lfNearestEnemyDistance standStill
  , lfNumNearbyEnemies standStill
  , lfStunAvailable standStill
  , lfDrillsRemaining standStill
  , zeroIfNull (lfOnActiveEnemy <$> up)
  , zeroIfNull (lfShortestPathLength <$> up)
  , zeroIfNull (lfManhattanDistance <$> up)
  , zeroIfNull (lfEnemiesOnPath <$> up)
  , zeroIfNull (lfNearestEnemyDistance <$> up)
  , zeroIfNull (lfNumNearbyEnemies <$> up)
  , zeroIfNull (lfStunAvailable <$> up)
  , zeroIfNull (lfDrillsRemaining <$> up)
  , zeroIfNull (lfOnActiveEnemy <$> right)
  , zeroIfNull (lfShortestPathLength <$> right)
  , zeroIfNull (lfManhattanDistance <$> right)
  , zeroIfNull (lfEnemiesOnPath <$> right)
  , zeroIfNull (lfNearestEnemyDistance <$> right)
  , zeroIfNull (lfNumNearbyEnemies <$> right)
  , zeroIfNull (lfStunAvailable <$> right)
  , zeroIfNull (lfDrillsRemaining <$> right)
  , zeroIfNull (lfOnActiveEnemy <$> down)
  , zeroIfNull (lfShortestPathLength <$> down)
  , zeroIfNull (lfManhattanDistance <$> down)
  , zeroIfNull (lfEnemiesOnPath <$> down)
  , zeroIfNull (lfNearestEnemyDistance <$> down)
  , zeroIfNull (lfNumNearbyEnemies <$> down)
  , zeroIfNull (lfStunAvailable <$> down)
  , zeroIfNull (lfDrillsRemaining <$> down)
  , zeroIfNull (lfOnActiveEnemy <$> left)
  , zeroIfNull (lfShortestPathLength <$> left)
  , zeroIfNull (lfManhattanDistance <$> left)
  , zeroIfNull (lfEnemiesOnPath <$> left)
  , zeroIfNull (lfNearestEnemyDistance <$> left)
  , zeroIfNull (lfNumNearbyEnemies <$> left)
  , zeroIfNull (lfStunAvailable <$> left)
  , zeroIfNull (lfDrillsRemaining <$> left)
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
