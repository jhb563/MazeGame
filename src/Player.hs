module Player where

import qualified Data.Array as Array
import Data.Maybe (catMaybes)
import Data.List (find, maximumBy)
import qualified Data.Set as Set
import System.Random (StdGen, randomR)

import MazeUtils
import Types
import WorldMutators (applyPlayerMove)

makePlayerMove :: World -> PlayerMove
makePlayerMove w = bestMove
  where
    allMoves = possibleMoves w
    possibleWorlds = applyPlayerMove w <$> allMoves
    scores = evaluateWorld <$> possibleWorlds
    movesWithScores = zip allMoves scores
    bestMove = fst $ maximumBy (\(_, score1) (_, score2) -> compare score1 score2) movesWithScores

possibleMoves :: World -> [PlayerMove]
possibleMoves w = baseMoves ++ stunMoves
  where
    standStillMove = PlayerMove DirectionNone False DirectionNone
    player = worldPlayer w
    bounds = (worldBoundaries w) Array.! (playerLocation player)

    possibleMove :: (CellBoundaries -> BoundaryType) -> MoveDirection -> Maybe PlayerMove
    possibleMove boundaryFunc direction = case boundaryFunc bounds of
      WorldBoundary -> Nothing
      Wall _ -> if playerDrillsRemaining player > 0
        then Just $ PlayerMove direction False direction
        else Nothing
      AdjacentCell _ -> Just $ PlayerMove direction False DirectionNone

    upMove = possibleMove upBoundary DirectionUp
    rightMove = possibleMove rightBoundary DirectionRight
    downMove = possibleMove downBoundary DirectionDown
    leftMove = possibleMove leftBoundary DirectionLeft

    baseMoves = standStillMove : (catMaybes [upMove, rightMove, downMove, leftMove])

    stunMoves = if playerCurrentStunDelay player /= 0 then []
      else [ m { activateStun = True } | m <- baseMoves ]

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
    standStill = produceLocationFeatures currentLoc w False
    up = case upBoundary bounds of
      WorldBoundary -> Nothing
      (AdjacentCell l) -> Just $ produceLocationFeatures l w False
      (Wall l) -> Just $ produceLocationFeatures l w True
    right = case rightBoundary bounds of
      WorldBoundary -> Nothing
      (AdjacentCell l) -> Just $ produceLocationFeatures l w False
      (Wall l) -> Just $ produceLocationFeatures l w True
    down = case downBoundary bounds of
      WorldBoundary -> Nothing
      (AdjacentCell l) -> Just $ produceLocationFeatures l w False
      (Wall l) -> Just $ produceLocationFeatures l w True
    left = case leftBoundary bounds of
      WorldBoundary -> Nothing
      (AdjacentCell l) -> Just $ produceLocationFeatures l w False
      (Wall l) -> Just $ produceLocationFeatures l w True

data LocationFeatures = LocationFeatures
  { lfOnActiveEnemy :: Int
  , lfShortestPathLength :: Int
  , lfManhattanDistance :: Int
  , lfEnemiesOnPath :: Int
  , lfNearestEnemyDistance :: Int
  , lfNumNearbyEnemies :: Int
  , lfStunAvailable :: Int
  , lfDrillsRemaining :: Int
  , lfMoveEase :: Int
  }

produceLocationFeatures :: Location -> World -> Bool -> LocationFeatures
produceLocationFeatures location@(lx, ly) w needsDrill = LocationFeatures
  (if onActiveEnemy then 1 else 0)
  shortestPathLength
  manhattanDistance
  enemiesOnPath
  nearestEnemyDistance
  numNearbyEnemies
  (if stunAvailable then 1 else 0)
  (fromIntegral drillsRemaining)
  moveEase
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
    moveEase = if not needsDrill then 2
      else if drillsRemaining > 0 then 1 else 0

evaluateWorld :: World -> Float
evaluateWorld w =
  onActiveEnemyScore +
  enemiesOnPathScore +
  shortestPathScore +
  manhattanDistanceScore +
  nearestEnemyDistanceScore +
  stunAvailableScore +
  numNearbyEnemiesScore +
  drillsRemainingScore
  where
    player = worldPlayer w
    playerLoc@(px, py) = playerLocation player
    radius = stunRadius . playerGameParameters . worldParameters $ w
    goalLoc@(gx, gy) = endLocation w
    activeEnemyLocations = enemyLocation <$>
      (filter (\e -> enemyCurrentStunTimer e == 0) (worldEnemies w))

    onActiveEnemy = playerLocation player `elem` activeEnemyLocations
    onActiveEnemyScore = if onActiveEnemy then -1000.0 else 0.0

    shortestPath = getShortestPath (worldBoundaries w) playerLoc goalLoc
    enemiesOnPath = length $ filter (\l -> Set.member l (Set.fromList activeEnemyLocations)) shortestPath
    enemiesOnPathScore = -85.0 * (fromIntegral enemiesOnPath)

    shortestPathLength = length shortestPath
    shortestPathScore = 1000.0 - (20.0 * (fromIntegral shortestPathLength))

    nearestEnemyDistance = length $ getShortestPathToTargetsWithLimit
      (worldBoundaries w) playerLoc (Set.fromList activeEnemyLocations) (Just 4)
    nearestEnemyDistanceScore = if nearestEnemyDistance == 0 || stunAvailable then 0.0
      else -100.0 * (fromIntegral (5 - nearestEnemyDistance))

    manhattanDistance = abs (gx - px) + abs (gy - py)
    manhattanDistanceScore = (-5.0) * (fromIntegral manhattanDistance)

    stunAvailable = playerCurrentStunDelay player == 0
    stunAvailableScore = if stunAvailable then 80.0 else 0.0

    numNearbyEnemies = length
      [ el | el@(elx, ely) <- activeEnemyLocations,
        abs (elx - px) <= radius && abs (ely - py) <= radius ]
    numNearbyEnemiesScore = -5.0 * (fromIntegral numNearbyEnemies)

    drillsRemaining = playerDrillsRemaining player
    drillsRemainingScore = 30.0 * (fromIntegral drillsRemaining)

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
