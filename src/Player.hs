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

makePlayerMove :: World -> PlayerMove
makePlayerMove w = PlayerMove finalMoveDirection useStun drillDirection
  where
    currentPlayer = worldPlayer w
    playerLoc = playerLocation currentPlayer
    maze = worldBoundaries w
    shortestPath = getShortestPathWithDrills
      maze
      (playerDrillsRemaining currentPlayer)
      (Set.fromList $ worldDrillPowerUpLocations w)
      playerLoc
      (endLocation w)
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

    (finalMoveDirection, useStun) = if not enemyClose
      then (shortestPathMoveDirection, False)
      else if canStun
        then (shortestPathMoveDirection, True)
        else case find (/= shortestPathMoveLocation) possibleMoves of
          Nothing -> (DirectionNone, False)
          Just l -> (getMoveDirection playerLoc l, False)

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
