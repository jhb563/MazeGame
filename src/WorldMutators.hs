module WorldMutators where

import qualified Data.Array as Array
import Data.List (delete)

import Types

applyPlayerMove :: World -> PlayerMove -> World
applyPlayerMove w move = worldAfterMove
  where
    player = worldPlayer w
    currentLoc = playerLocation player

    worldAfterDrill = modifyWorldForPlayerDrill w (drillDirection move)

    worldAfterStun = if activateStun move
      then modifyWorldForStun worldAfterDrill
      else worldAfterDrill

    newLocation = nextLocationForMove (worldBoundaries w Array.! currentLoc) currentLoc (playerMoveDirection move)
    worldAfterMove = modifyWorldForPlayerMove worldAfterStun newLocation

-- Check for invalid moves like using the stun when we can't
applyPlayerMove' :: PlayerMove -> World -> (World, Bool)
applyPlayerMove' move w = if isValidMove
  then (worldAfterMove, True)
  else (w, False)
  where
    player = worldPlayer w
    currentLoc = playerLocation player

    worldAfterDrill = modifyWorldForPlayerDrill w (drillDirection move)

    worldAfterStun = if activateStun move
      then modifyWorldForStun worldAfterDrill
      else worldAfterDrill

    newLocation = nextLocationForMove (worldBoundaries worldAfterDrill Array.! currentLoc) currentLoc (playerMoveDirection move)

    isValidStunUse = if activateStun move then playerCurrentStunDelay player == 0 else True
    isValidMovement = playerMoveDirection move == DirectionNone || newLocation /= currentLoc
    isValidMove = isValidStunUse && isValidMovement

    worldAfterMove = modifyWorldForPlayerMove worldAfterStun newLocation

modifyWorldForPlayerMove :: World -> Location -> World
modifyWorldForPlayerMove w newLoc = w
  { worldPlayer = finalPlayer
  , worldDrillPowerUpLocations = finalDrillList
  }
  where
    currentPlayer = worldPlayer w
    playerAfterMove = movePlayer newLoc currentPlayer
    drillLocs = worldDrillPowerUpLocations w
    (finalPlayer, finalDrillList) = if newLoc `elem` drillLocs
      then (pickupDrill playerAfterMove, delete newLoc drillLocs)
      else (playerAfterMove, drillLocs)


modifyWorldForStun :: World -> World
modifyWorldForStun w = w { worldPlayer = newPlayer, worldEnemies = newEnemies, stunCells = stunAffectedCells}
  where
    pl = worldPlayer w
    playerParams = playerGameParameters . worldParameters $ w
    nextStunTimer = playerNextStunDelay pl
    newNextStun = min (stunTimerMax playerParams) (nextStunTimer + (stunTimerIncrease playerParams))
    newPlayer = pl { playerCurrentStunDelay = nextStunTimer, playerNextStunDelay = newNextStun }

    newEnemies = stunEnemyIfClose <$> worldEnemies w

    stunAffectedCells :: [Location]
    stunAffectedCells =
      let (cx, cy) = playerLocation pl
          r = stunRadius . playerGameParameters . worldParameters $ w
          worldRows = numRows . worldParameters $ w
          worldCols = numColumns . worldParameters $ w
      in  [(x,y) | x <- [(cx - r)..(cx + r)], y <- [(cy - r)..(cy + r)], x >= 0 && x < worldCols, y >= 0 && y < worldRows]

    stunEnemyIfClose :: Enemy -> Enemy
    stunEnemyIfClose e = if enemyLocation e `elem` stunAffectedCells
      then stunEnemy e (enemyGameParameters . worldParameters $ w)
      else e

-- DRILLING

modifyWorldForPlayerDrill :: World -> MoveDirection -> World
modifyWorldForPlayerDrill w drillDirection = case drillDirection of
  DirectionUp -> drillLocation upBoundary breakUpWall breakDownWall w
  DirectionRight -> drillLocation rightBoundary breakRightWall breakLeftWall w
  DirectionDown -> drillLocation downBoundary breakDownWall breakUpWall w
  DirectionLeft -> drillLocation leftBoundary breakLeftWall breakRightWall w
  DirectionNone -> w

drillLocation
  :: (CellBoundaries -> BoundaryType)
  -> (CellBoundaries -> CellBoundaries)
  -> (CellBoundaries -> CellBoundaries)
  -> World
  -> World
drillLocation boundaryFunc breakFunc1 breakFunc2 w =
  case (playerDrillsRemaining currentPlayer > 0, boundaryFunc cellBounds) of
    (True, Wall location2) ->
      let newPlayer = activatePlayerDrill currentPlayer
          newBounds1 = breakFunc1 cellBounds
          newBounds2 = breakFunc2 (worldBounds Array.! location2)
          newMaze = worldBounds Array.// [(currentLocation, newBounds1), (location2, newBounds2)]
      in  w { worldPlayer = newPlayer, worldBoundaries = newMaze }
    _ -> w
  where
    currentPlayer = worldPlayer w
    currentLocation = playerLocation currentPlayer
    worldBounds = worldBoundaries w
    cellBounds = worldBounds Array.! currentLocation

breakUpWall :: CellBoundaries -> CellBoundaries
breakUpWall cb = case upBoundary cb of
  (Wall adjacentLoc) -> cb {upBoundary = AdjacentCell adjacentLoc}
  _ -> error "Can't break wall"

breakRightWall :: CellBoundaries -> CellBoundaries
breakRightWall cb = case rightBoundary cb of
  (Wall adjacentLoc) -> cb {rightBoundary = AdjacentCell adjacentLoc}
  _ -> error "Can't break wall"

breakDownWall :: CellBoundaries -> CellBoundaries
breakDownWall cb = case downBoundary cb of
  (Wall adjacentLoc) -> cb {downBoundary = AdjacentCell adjacentLoc}
  _ -> error "Can't break wall"

breakLeftWall :: CellBoundaries -> CellBoundaries
breakLeftWall cb = case leftBoundary cb of
  (Wall adjacentLoc) -> cb {leftBoundary = AdjacentCell adjacentLoc}
  _ -> error "Can't break wall"

-- PLAYER MODIFIERS
activatePlayerDrill :: Player -> Player
activatePlayerDrill pl = pl
  { playerDrillsRemaining = decrementIfPositive (playerDrillsRemaining pl)}

pickupDrill :: Player -> Player
pickupDrill pl = pl
  { playerDrillsRemaining = (playerDrillsRemaining pl) + 1}

movePlayer :: Location -> Player -> Player
movePlayer newLoc pl = pl
  { playerLocation = newLoc }

-- ENEMY MODIFIERS
stunEnemy :: Enemy -> EnemyGameParameters -> Enemy
stunEnemy (Enemy loc lag nextStun _) params = Enemy loc newLag newNextStun nextStun
  where
    newNextStun = max (minStunTime params) (nextStun - (stunTimeDecrease params))
    newLag = max (minLagTime params) (lag - 1)

-- GENERIC MODIFIERS

decrementIfPositive :: Word -> Word
decrementIfPositive 0 = 0
decrementIfPositive x = x - 1

nextLocationForMove :: CellBoundaries -> Location -> MoveDirection -> Location
nextLocationForMove bounds currentLoc choice = case choice of
  DirectionUp -> case upBoundary bounds of
    AdjacentCell l -> l
    _ -> currentLoc
  DirectionRight -> case rightBoundary bounds of
    AdjacentCell l -> l
    _ -> currentLoc
  DirectionDown -> case downBoundary bounds of
    AdjacentCell l -> l
    _ -> currentLoc
  DirectionLeft -> case leftBoundary bounds of
    AdjacentCell l -> l
    _ -> currentLoc
  DirectionNone -> currentLoc

