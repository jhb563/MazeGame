module Player where

import qualified Data.Array as Array
import Data.Maybe (catMaybes)
import Data.List (find, maximumBy)
import qualified Data.Set as Set
import System.Random (StdGen, randomR)

import MazeUtils (getAdjacentLocations, getShortestPath, getShortestPathWithDrills)
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

evaluateWorld :: World -> Float
evaluateWorld w =
  onActiveEnemyScore +
  shortestPathScore +
  manhattanDistanceScore +
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

    shortestPathLength = length $
      getShortestPath (worldBoundaries w) playerLoc goalLoc
    shortestPathScore = 1000.0 - (20.0 * (fromIntegral shortestPathLength))

    manhattanDistance = abs (gx - px) + abs (gy - py)
    manhattanDistanceScore = (-5.0) * (fromIntegral manhattanDistance)

    stunAvailable = playerCurrentStunDelay player == 0
    stunAvailableScore = if stunAvailable then 80.0 else 0.0

    numNearbyEnemies = length
      [ el | el@(elx, ely) <- activeEnemyLocations,
        abs (elx - px) <= radius && abs (ely - py) <= radius ]
    numNearbyEnemiesScore = -100.0 * (fromIntegral numNearbyEnemies)

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