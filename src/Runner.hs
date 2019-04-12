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
      enemies = Enemy <$> randomLocations
      initialWorld = World (0, 0) (0,0) (24,24) maze GameInProgress gen'' enemies
  play
    windowDisplay
    white
    1
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
    (px, py) = cellCenter (conversion (playerLocation world))
    playerMarker = translate px py (Circle 10)

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
      in  [ drawEdge (tr, tl, (tlx, tly - 2), (trx, try - 2)) up
          , drawEdge (br, tr, (trx-2, try), (brx-2, bry)) right
          , drawEdge (bl, br, (brx, bry+2), (blx, bly+2)) down
          , drawEdge (tl, bl, (blx+2, bly), (tlx+2, tly)) left
          ]

    drawEdge :: (Point, Point, Point, Point) -> BoundaryType -> Picture
    drawEdge (p1, p2, _, _) (AdjacentCell _) = Line [p1, p2]
    drawEdge (p1, p2, p3, p4) _ = Color blue (Polygon [p1, p2, p3, p4])

    enemyPic :: Enemy -> Picture
    enemyPic (Enemy loc) =
      let (centerX, centerY) = cellCenter $ conversion loc
          tl = (centerX - 5, centerY + 5)
          tr = (centerX + 5, centerY + 5)
          br = (centerX + 5, centerY - 5)
          bl = (centerX - 5, centerY - 5)
      in  Color orange (Polygon [tl, tr, br, bl])

inputHandler :: Event -> World -> World
inputHandler event w
  | worldResult w == GameWon = case event of
      (EventKey (SpecialKey KeyEnter) Down _ _) ->
        let (newMaze, gen') = generateRandomMaze (worldRandomGenerator w) (25, 25)
            (newLocations, gen'') = runState
              (replicateM (length (worldEnemies w)) (generateRandomLocation (25, 25)))
              gen'
        in  World (0,0) (0,0) (24, 24) newMaze GameInProgress gen'' (Enemy <$> newLocations)
      _ -> w
  | worldResult w == GameLost = case event of
      (EventKey (SpecialKey KeyEnter) Down _ _) ->
        let (newLocations, gen') = runState
              (replicateM (length (worldEnemies w)) (generateRandomLocation (25, 25)))
              (worldRandomGenerator w)
        in  World (0,0) (0,0) (24, 24) (worldBoundaries w) GameInProgress gen' (Enemy <$> newLocations)
      _ -> w
  | otherwise = case event of
      (EventKey (SpecialKey KeyUp) Down _ _) -> w { playerLocation = nextLocation upBoundary }
      (EventKey (SpecialKey KeyDown) Down _ _) -> w { playerLocation = nextLocation downBoundary }
      (EventKey (SpecialKey KeyRight) Down _ _) -> w { playerLocation = nextLocation rightBoundary }
      (EventKey (SpecialKey KeyLeft) Down _ _) -> w { playerLocation = nextLocation leftBoundary }
      _ -> w
  where
    cellBounds = (worldBoundaries w) Array.! (playerLocation w)

    nextLocation :: (CellBoundaries -> BoundaryType) -> Location
    nextLocation boundaryFunc = case boundaryFunc cellBounds of
      (AdjacentCell cell) -> cell
      _ -> playerLocation w

updateFunc :: Float -> World -> World
updateFunc _ w
  | playerLocation w == endLocation w = w { worldResult = GameWon }
  | playerLocation w `elem` (enemyLocation <$> worldEnemies w) = w { worldResult = GameLost }
  | otherwise = w { worldRandomGenerator = newGen, worldEnemies = newEnemies }
  where
    (newEnemies, newGen) = runState
      (sequence (updateEnemy (worldBoundaries w) (playerLocation w) <$> worldEnemies w))
      (worldRandomGenerator w)

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

updateEnemy :: Maze -> Location -> Enemy -> State StdGen Enemy
updateEnemy maze playerLocation e@(Enemy location) = if (null potentialLocs)
  then return e
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
    return (Enemy newLocation)
  where
    potentialLocs = getAdjacentLocations maze location

generateRandomLocation :: (Int, Int) -> State StdGen Location
generateRandomLocation (numCols, numRows) = do
  gen <- get
  let (randomCol, gen') = randomR (0, numCols - 1) gen
      (randomRow, gen'') = randomR (0, numRows - 1) gen'
  put gen''
  return (randomCol, randomRow)
