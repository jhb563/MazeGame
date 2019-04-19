module Runner where

import qualified Data.Array as Array
import Control.Monad.State (State, get, put, runState, replicateM)
import Data.Ix (range)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust, catMaybes)
import System.Random (getStdGen, StdGen, randomR)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import MazeParser (generateRandomMaze, sampleMaze)
import MazeUtils (getAdjacentLocations, getShortestPath)
import Types

import Debug.Trace

globalCellSize :: Float
globalCellSize = 25

globalXOffset :: Float
globalXOffset = -300

globalYOffset :: Float
globalYOffset = -300

maxCellIndex :: Int
maxCellIndex = 24

windowDisplay :: Display
windowDisplay = InWindow "Window" (625, 625) (10, 10)

boundariesMap :: (Int, Int) -> Maze
boundariesMap (numColumns, numRows) = Array.array
  ((0,0), (numRows - 1, numColumns - 1))
  (buildBounds <$> (range ((0,0), (numColumns - 1, numRows - 1))))
  where
    buildBounds :: Location -> (Location, CellBoundaries)
    buildBounds loc = (loc, simpleBoundaries (numColumns, numRows) loc)

simpleBoundaries :: (Int, Int) -> Location -> CellBoundaries
simpleBoundaries (numColumns, numRows) (x, y) = CellBoundaries
  (if y + 1 < numRows then AdjacentCell (x, y+1) else WorldBoundary)
  (if x + 1 < numColumns then AdjacentCell (x+1, y) else WorldBoundary)
  (if y > 0 then AdjacentCell (x, y-1) else WorldBoundary)
  (if x > 0 then AdjacentCell (x-1, y) else WorldBoundary)

main :: IO ()
main = do
  gen <- getStdGen
  let (maze, gen') = generateRandomMaze gen (25, 25)
      numEnemies = 4
      (randomLocations, gen'') = runState
        (replicateM numEnemies (generateRandomLocation (25,25)))
        gen'
      enemies = mkNewEnemy <$> randomLocations
      initialWorld = World newPlayer (0,0) (24,24) maze GameInProgress gen'' enemies [] 0
  play
    windowDisplay
    white
    20
    initialWorld
    (drawingFunc (globalXOffset, globalYOffset) globalCellSize)
    inputHandler
    updateFunc

-- First argument is offset from true 0,0 to the center of the grid space 0,0
drawingFunc :: (Float, Float) -> Float -> World -> Picture
drawingFunc (xOffset, yOffset) cellSize world
 | worldResult world == GameWon = Translate (-275) 0 $ Scale 0.12 0.25
     (Text "Congratulations! You've won! Press enter to restart with a new maze!")
 | worldResult world == GameLost = Translate (-275) 0 $ Scale 0.12 0.25
     (Text "Oh no! You've lost! Press enter to restart this maze!")
 | otherwise = Pictures
    [mapGrid, startPic, endPic, playerMarker, Pictures (enemyPic <$> worldEnemies world)]
  where
    conversion = locationToCoords (xOffset, yOffset) cellSize
    (px, py) = cellCenter (conversion (playerLocation (worldPlayer world)))

    stunReadyCircle = if playerCurrentStunDelay (worldPlayer world) == 0
      then Color red (Circle 5)
      else Blank
    playerMarker = translate px py (Pictures [stunReadyCircle, Circle 10])

    startCoords = conversion (startLocation world)
    endCoords = conversion (endLocation world)
    startPic = Color blue (Polygon
      [ cellTopLeft startCoords
      , cellTopRight startCoords
      , cellBottomRight startCoords
      , cellBottomLeft startCoords
      ])
    endPic = Color green (Polygon
      [ cellTopLeft endCoords
      , cellTopRight endCoords
      , cellBottomRight endCoords
      , cellBottomLeft endCoords
      ])

    mapGrid = Pictures $ concatMap makeWallPictures (Array.assocs (worldBoundaries world))

    makeWallPictures :: (Location, CellBoundaries) -> [Picture]
    makeWallPictures ((x,y), CellBoundaries up right down left) =
      let coords = conversion (x,y)
          tl@(tlx, tly) = cellTopLeft coords
          tr@(trx, try) = cellTopRight coords
          bl@(blx, bly) = cellBottomLeft coords
          br@(brx, bry) = cellBottomRight coords
          stunBackground = if (x, y) `elem` stunCells world
            then Color cyan (Polygon [tl, tr, br, bl])
            else Blank
      in  [ stunBackground
          , drawEdge (tr, tl, (tlx, tly - 2), (trx, try - 2)) up
          , drawEdge (br, tr, (trx-2, try), (brx-2, bry)) right
          , drawEdge (bl, br, (brx, bry+2), (blx, bly+2)) down
          , drawEdge (tl, bl, (blx+2, bly), (tlx+2, tly)) left
          ]

    drawEdge :: (Point, Point, Point, Point) -> BoundaryType -> Picture
    drawEdge (p1, p2, _, _) (AdjacentCell _) = Line [p1, p2]
    drawEdge (p1, p2, p3, p4) _ = Color blue (Polygon [p1, p2, p3, p4])

    enemyPic :: Enemy -> Picture
    enemyPic (Enemy loc _ _ currentStun) =
      let (centerX, centerY) = cellCenter $ conversion loc
          tl = (centerX - 5, centerY + 5)
          tr = (centerX + 5, centerY + 5)
          br = (centerX + 5, centerY - 5)
          bl = (centerX - 5, centerY - 5)
          enemyColor = if currentStun == 0 then orange else yellow
      in  Color enemyColor (Polygon [tl, tr, br, bl])

inputHandler :: Event -> World -> World
inputHandler event w
  | worldResult w == GameWon = case event of
      (EventKey (SpecialKey KeyEnter) Down _ _) ->
        let (newMaze, gen') = generateRandomMaze (worldRandomGenerator w) (25, 25)
            (newLocations, gen'') = runState
              (replicateM (length (worldEnemies w)) (generateRandomLocation (25, 25)))
              gen'
        in  World newPlayer (0,0) (24, 24) newMaze GameInProgress gen''
              (mkNewEnemy <$> newLocations) [] 0
      _ -> w
  | worldResult w == GameLost = case event of
      (EventKey (SpecialKey KeyEnter) Down _ _) ->
        let (newLocations, gen') = runState
              (replicateM (length (worldEnemies w)) (generateRandomLocation (25, 25)))
              (worldRandomGenerator w)
        in  World newPlayer (0,0) (24, 24) (worldBoundaries w) GameInProgress gen'
              (mkNewEnemy <$> newLocations) [] 0
      _ -> w
  | otherwise = case event of
      (EventKey (SpecialKey KeyUp) Down _ _) -> w
        { worldPlayer = currentPlayer { playerLocation = nextLocation upBoundary } }
      (EventKey (SpecialKey KeyDown) Down _ _) -> w
        { worldPlayer = currentPlayer { playerLocation = nextLocation downBoundary } }
      (EventKey (SpecialKey KeyRight) Down _ _) -> w
        { worldPlayer = currentPlayer { playerLocation = nextLocation rightBoundary } }
      (EventKey (SpecialKey KeyLeft) Down _ _) -> w
        { worldPlayer = currentPlayer { playerLocation = nextLocation leftBoundary } }
      (EventKey (SpecialKey KeySpace) Down _ _) -> if playerCurrentStunDelay currentPlayer /= 0 then w
        else w
          { worldPlayer = activatePlayerStun currentPlayer
          , worldEnemies = stunEnemyIfClose <$> worldEnemies w
          , stunCells = stunAffectedCells
          }
      _ -> w
  where
    cellBounds = (worldBoundaries w) Array.! (playerLocation (worldPlayer w))
    currentPlayer = worldPlayer w

    nextLocation :: (CellBoundaries -> BoundaryType) -> Location
    nextLocation boundaryFunc = case boundaryFunc cellBounds of
      (AdjacentCell cell) -> cell
      _ -> playerLocation currentPlayer

    stunAffectedCells :: [Location]
    stunAffectedCells =
      let (cx, cy) = playerLocation currentPlayer
      in  [(x,y) | x <- [(cx-2)..(cx+2)], y <- [(cy-2)..(cy+2)], x >= 0 && x <= 24, y >= 0 && y <= 24]

    stunEnemyIfClose :: Enemy -> Enemy
    stunEnemyIfClose e = if enemyLocation e `elem` stunAffectedCells
      then stunEnemy e
      else e

updateFunc :: Float -> World -> World
updateFunc _ w
  | playerLocation player == endLocation w = w { worldResult = GameWon }
  | playerLocation player `elem` activeEnemyLocations = w { worldResult = GameLost }
  | otherwise = (clearStunCells . incrementWorldTime)
    (w { worldPlayer = newPlayer, worldRandomGenerator = newGen, worldEnemies = newEnemies })
  where
    player = worldPlayer w
    newPlayer = updatePlayerOnTick player
    (newEnemies, newGen) = runState
      (sequence (updateEnemy (worldTime w) (worldBoundaries w) (playerLocation player) <$> worldEnemies w))
      (worldRandomGenerator w)
    activeEnemyLocations = enemyLocation <$> filter (\e -> enemyCurrentStunTimer e == 0) (worldEnemies w)

-- Given a discrete location and some offsets, determine all the coordinates of the cell.
locationToCoords :: (Float, Float) -> Float -> Location -> CellCoordinates
locationToCoords (xOffset, yOffset) cellSize (x, y) = CellCoordinates
  (centerX, centerY) -- Center
  (centerX - halfCell, centerY + halfCell) -- Top Left
  (centerX + halfCell, centerY + halfCell) -- Top Right
  (centerX - halfCell, centerY - halfCell) -- Bottom Left
  (centerX + halfCell, centerY - halfCell) -- Bottom Right
  where
    (centerX, centerY) = (xOffset + (fromIntegral x) * cellSize, yOffset + (fromIntegral y) * cellSize)
    halfCell = cellSize / 2.0

updatePlayerOnTick :: Player -> Player
updatePlayerOnTick p = p { playerCurrentStunDelay = decrementIfPositive (playerCurrentStunDelay p)}

-- TODO
updateEnemy :: Word -> Maze -> Location -> Enemy -> State StdGen Enemy
updateEnemy time maze playerLocation e@(Enemy location lagTime nextStun currentStun) = if not shouldUpdate
  then return $ e {enemyCurrentStunTimer = decrementIfPositive currentStun}
  else do
    gen <- get
    let (randomMoveRoll, gen') = randomR (1 :: Int, 5) gen
    let (newLocation, newGen) = if randomMoveRoll == 1
          then
            let (randomIndex, newGen) = randomR (0, (length potentialLocs) - 1) gen'
            in  (potentialLocs !! randomIndex, newGen)
          else
            let shortestPath = getShortestPath maze location playerLocation
            in  (if null shortestPath then location else head shortestPath, gen')
    put newGen
    return (Enemy newLocation lagTime nextStun (decrementIfPositive currentStun))
  where
    isUpdateTick = time `mod` lagTime == 0
    shouldUpdate = isUpdateTick && currentStun == 0 && not (null potentialLocs)
    potentialLocs = getAdjacentLocations maze location

generateRandomLocation :: (Int, Int) -> State StdGen Location
generateRandomLocation (numCols, numRows) = do
  gen <- get
  let (randomCol, gen') = randomR (0, numCols - 1) gen
      (randomRow, gen'') = randomR (0, numRows - 1) gen'
  put gen''
  return (randomCol, randomRow)

-- Initializers

mkNewEnemy :: Location -> Enemy
mkNewEnemy loc = Enemy loc 20 60 0

newPlayer :: Player
newPlayer = Player (0, 0) 0 200

-- Mutators

incrementWorldTime :: World -> World
incrementWorldTime w = w { worldTime = worldTime w + 1 }

clearStunCells :: World -> World
clearStunCells w = w { stunCells = []}

activatePlayerStun :: Player -> Player
activatePlayerStun (Player loc _ nextStunTimer) = Player loc nextStunTimer (nextStunTimer + 10)

stunEnemy :: Enemy -> Enemy
stunEnemy (Enemy loc lag nextStun _) = Enemy loc newLag newNextStun nextStun
  where
    newNextStun = max 20 (nextStun - 5)
    newLag = max 10 (lag - 1)

decrementIfPositive :: Word -> Word
decrementIfPositive 0 = 0
decrementIfPositive x = x - 1
