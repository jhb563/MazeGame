module Player where

import Data.List (find)
import qualified Data.Set as Set
import System.Random (StdGen, randomR)

import MazeUtils (getAdjacentLocations, getShortestPath)
import Types

data MoveChoice = MoveUp | MoveRight | MoveDown | MoveLeft | StandStill

data PlayerMove = PlayerMove
  { playerMoveChoice :: MoveChoice
  , activateStun :: Bool
  }

makePlayerMove :: World -> PlayerMove
makePlayerMove w = PlayerMove finalMoveChoice useStun
  where
    currentPlayer = worldPlayer w
    playerLoc = playerLocation currentPlayer
    maze = worldBoundaries w
    shortestPath = getShortestPath maze playerLoc (endLocation w)
    shortestPathMoveLocation = if null shortestPath
      then playerLoc
      else (head shortestPath)
    shortestPathMoveChoice = getMoveChoice playerLoc shortestPathMoveLocation

    activeEnemyLocs = Set.fromList
      (enemyLocation <$>
        (filter (\e -> enemyCurrentStunTimer e == 0) (worldEnemies w)))
    radius = stunRadius . playerGameParameters . worldParameters $ w
    enemyClose = any (\l -> Set.member l activeEnemyLocs) (take radius shortestPath)

    canStun = playerCurrentStunDelay currentPlayer == 0

    possibleMoves = getAdjacentLocations maze playerLoc

    (finalMoveChoice, useStun) = if not enemyClose
      then (shortestPathMoveChoice, False)
      else if canStun
        then (shortestPathMoveChoice, True)
        else case find (/= shortestPathMoveLocation) possibleMoves of
          Nothing -> (StandStill, False)
          Just l -> (getMoveChoice playerLoc l, False)

data EnemyMove = EnemyMove
  { enemyMoveChoice :: MoveChoice
  , newRandomGenerator :: StdGen
  }

makeEnemyMove :: World -> Enemy -> StdGen -> EnemyMove
makeEnemyMove w e gen = EnemyMove (getMoveChoice enemyLoc newLocation) newGen
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

getMoveChoice :: Location -> Location -> MoveChoice
getMoveChoice (x1, y1) (x2, y2)
  | y2 == y1 + 1 = MoveUp
  | x2 == x1 + 1 = MoveRight
  | y2 == y1 - 1 = MoveDown
  | x2 == x1 - 1 = MoveLeft
  | otherwise = StandStill
