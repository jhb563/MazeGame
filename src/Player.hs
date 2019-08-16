module Player where

import qualified Data.Array as Array
import Data.List (find)
import qualified Data.Set as Set
import System.Random (StdGen, randomR)

import MazeUtils (getAdjacentLocations, getShortestPath, getShortestPathWithDrills)
import Types

data MoveDirection =
  DirectionUp |
  DirectionRight |
  DirectionDown |
  DirectionLeft |
  DirectionNone

data PlayerMove = PlayerMove
  { playerMoveDirection :: MoveDirection
  , activateStun :: Bool
  , drillDirection :: MoveDirection
  }

makePlayerMove :: World -> (PlayerMove, PlayerMemory)
makePlayerMove w = 
  ( PlayerMove finalMoveDirection useStun drillDirection
  , if emptyCache then (PlayerMemory Nothing) else memoryFromMove
  )
  where
    currentPlayer = worldPlayer w
    playerLoc = playerLocation currentPlayer
    maze = worldBoundaries w
    (useCache, cachePath) = case playerMemory currentPlayer of
      (PlayerMemory (Just (first : rest))) -> (first == playerLoc, rest)
      _ -> (False, [])
    shortestPath = if useCache then cachePath
      else getShortestPathWithDrills
        maze
        (playerDrillsRemaining currentPlayer)
        (Set.fromList $ worldDrillPowerUpLocations w)
        playerLoc
        (endLocation w)
    memoryFromMove = PlayerMemory (Just shortestPath)
    shortestPathMoveLocation = if null shortestPath
      then playerLoc
      else (head shortestPath)
    shortestPathMoveDirection = getMoveDirection playerLoc shortestPathMoveLocation

    locationBounds = maze Array.! playerLoc
    drillDirection = case shortestPathMoveDirection of
      DirectionUp -> case upBoundary locationBounds of
        Wall _ -> DirectionUp
        _ -> DirectionNone
      DirectionRight -> case rightBoundary locationBounds of
        Wall _ -> DirectionRight
        _ -> DirectionNone
      DirectionDown -> case downBoundary locationBounds of
        Wall _ -> DirectionDown
        _ -> DirectionNone
      DirectionLeft -> case leftBoundary locationBounds of
        Wall _ -> DirectionLeft
        _ -> DirectionNone
      DirectionNone -> DirectionNone

    activeEnemyLocs = Set.fromList
      (enemyLocation <$>
        (filter (\e -> enemyCurrentStunTimer e == 0) (worldEnemies w)))
    radius = stunRadius . playerGameParameters . worldParameters $ w
    enemyClose = any (\l -> Set.member l activeEnemyLocs) (take radius shortestPath)

    canStun = playerCurrentStunDelay currentPlayer == 0

    possibleMoves = getAdjacentLocations maze playerLoc

    (finalMoveDirection, useStun, emptyCache) = if not enemyClose
      then (shortestPathMoveDirection, False, False)
      else if canStun
        then (shortestPathMoveDirection, True, False)
        else case find (/= shortestPathMoveLocation) possibleMoves of
          Nothing -> (DirectionNone, False, True)
          Just l -> (getMoveDirection playerLoc l, False, True)

data EnemyMove = EnemyMove
  { enemyMoveDirection :: MoveDirection
  , newRandomGenerator :: StdGen
  }

makeEnemyMove :: World -> Enemy -> StdGen -> EnemyMove
makeEnemyMove w e gen = EnemyMove (getMoveDirection enemyLoc newLocation) newGen
  where
    playerLoc = playerLocation . worldPlayer $ w
    enemyLoc = enemyLocation e
    enemyParams = enemyGameParameters . worldParameters $ w
    maze = worldBoundaries w
    randomMoveChance = enemyRandomMoveChance enemyParams
    (randomMoveRoll, gen') = randomR (1, randomMoveChance) gen
    potentialLocs = getAdjacentLocations maze enemyLoc
    (newLocation, newGen) = if randomMoveRoll == 1
      then
        let (randomIndex, newGen) = randomR (0, (length potentialLocs) - 1) gen'
        in  (potentialLocs !! randomIndex, newGen)
      else
        let shortestPath = getShortestPath maze enemyLoc playerLoc
        in  (if null shortestPath then enemyLoc else head shortestPath, gen')

getMoveDirection :: Location -> Location -> MoveDirection
getMoveDirection (x1, y1) (x2, y2)
  | y2 == y1 + 1 = DirectionUp
  | x2 == x1 + 1 = DirectionRight
  | y2 == y1 - 1 = DirectionDown
  | x2 == x1 - 1 = DirectionLeft
  | otherwise = DirectionNone

data WorldFeatures = WorldFeatures
  { standStillFeatures :: LocationFeatures
  , moveUpFeatures :: Maybe LocationFeatures
  , moveRightFeatures :: Maybe LocationFeatures
  , moveDownFeatures :: Maybe LocationFeatures
  , moveLeftFeatures :: Maybe LocationFeatures
  }

produceWorldFeatures :: World -> WorldFeatures
produceWorldFeatures w = WorldFeatures standStill up right down left
  where
    currentLoc = playerLocation . worldPlayer $ w
    bounds = worldBoundaries w Array.! currentLoc
    standStill = produceLocationFeatures currentLoc w
    up = case upBoundary bounds of
      WorldBoundary -> Nothing
      (AdjacentCell l) -> Just $ produceLocationFeatures l w
      (Wall l) -> Just $ produceLocationFeatures l w
    right = case rightBoundary bounds of
      WorldBoundary -> Nothing
      (AdjacentCell l) -> Just $ produceLocationFeatures l w
      (Wall l) -> Just $ produceLocationFeatures l w
    down = case downBoundary bounds of
      WorldBoundary -> Nothing
      (AdjacentCell l) -> Just $ produceLocationFeatures l w
      (Wall l) -> Just $ produceLocationFeatures l w
    left = case leftBoundary bounds of
      WorldBoundary -> Nothing
      (AdjacentCell l) -> Just $ produceLocationFeatures l w
      (Wall l) -> Just $ produceLocationFeatures l w

data LocationFeatures = LocationFeatures
  { lfOnActiveEnemy :: Int
  , lfShortestPathLength :: Int
  , lfManhattanDistance :: Int
  , lfEnemiesOnPath :: Int
  , lfNearestEnemyDistance :: Int
  , lfNumNearbyEnemies :: Int
  , lfStunAvailable :: Int
  , lfDrillsRemaining :: Int
  }

produceLocationFeatures :: Location -> World -> LocationFeatures
produceLocationFeatures location@(lx, ly) w = LocationFeatures
  (if onActiveEnemy then 1 else 0)
  shortestPathLength
  manhattanDistance
  enemiesOnPath
  nearestEnemyDistance
  numNearbyEnemies
  (if stunAvailable then 1 else 0)
  (fromIntegral drillsRemaining)
  where
    player = worldPlayer w
    radius = stunRadius . playerGameParameters . worldParameters $ w
    goalLoc@(gx, gy) = endLocation w
    activeEnemyLocations = enemyLocation <$>
      (filter (\e -> enemyCurrentStunTimer e == 0) (worldEnemies w))

    onActiveEnemy = location `elem` activeEnemyLocations

    shortestPath = getShortestPath (worldBoundaries w) location goalLoc
    enemiesOnPath = length $ filter (\l -> Set.member l (Set.fromList activeEnemyLocations)) shortestPath

    shortestPathLength = length shortestPath

    nearestEnemyDistance = length $ getShortestPathToTargetsWithLimit
      (worldBoundaries w) location (Set.fromList activeEnemyLocations) (Just 4)

    manhattanDistance = abs (gx - lx) + abs (gy - ly)

    stunAvailable = playerCurrentStunDelay player == 0

    numNearbyEnemies = length
      [ el | el@(elx, ely) <- activeEnemyLocations,
        abs (elx - lx) <= radius && abs (ely - ly) <= radius ]

    drillsRemaining = playerDrillsRemaining player
