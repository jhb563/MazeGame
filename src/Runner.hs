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

windowDisplay :: RenderParameters -> Display
windowDisplay rp = InWindow "Window"
  (screenDimen rp, screenDimen rp)
  (screenOffsetX rp, screenOffsetY rp)

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
  let gameParams = defaultGameParameters
      renderParams = defaultRenderParameters
      (maze, gen') = generateRandomMaze gen (numRows gameParams, numColumns gameParams)
      (randomLocations, gen'') = runState
        (replicateM (numEnemies gameParams) (generateRandomLocation (numRows gameParams, numColumns gameParams)))
        gen'
      enemies = (mkNewEnemy (enemyGameParameters gameParams)) <$> randomLocations
      endCell = (numColumns gameParams - 1, numRows gameParams - 1)
      initialWorld = World (newPlayer (playerGameParameters gameParams)) (0,0) endCell maze GameInProgress gen'' enemies [] 0 gameParams
  play
    (windowDisplay renderParams)
    white
    (tickRate gameParams)
    initialWorld
    (drawingFunc renderParams)
    inputHandler
    updateFunc

-- First argument is offset from true 0,0 to the center of the grid space 0,0
drawingFunc :: RenderParameters -> World -> Picture
drawingFunc rp world
 | worldResult world == GameWon = Translate textOffsetX textOffsetY $ Scale textScaleX textScaleY
     (Text "Congratulations! You've won! Press enter to restart with a new maze!")
 | worldResult world == GameLost = Translate textOffsetX textOffsetY $ Scale textScaleX textScaleY
     (Text "Oh no! You've lost! Press enter to restart this maze!")
 | otherwise = Pictures
    [mapGrid, startPic, endPic, playerMarker, Pictures (enemyPic <$> worldEnemies world)]
  where
    (textOffsetX, textOffsetY) = textOffset rp
    (textScaleX, textScaleY) = textScale rp
    screenSize = fromIntegral . screenDimen $ rp
    cellSize = screenSize / (fromIntegral . numRows . worldParameters $ world)
    offset = (-1) * (screenSize / 2.0) + (cellSize / 2.0)
    conversion = locationToCoords (offset, offset) cellSize
    (px, py) = cellCenter (conversion (playerLocation (worldPlayer world)))

    playerRP = playerRenderParameters rp
    stunReadyCircle = if playerCurrentStunDelay (worldPlayer world) == 0
      then Color (playerStunIndicatorColor playerRP) (Circle (playerStunIndicatorSize playerRP))
      else Blank
    playerIndicator = Color (playerIndicatorColor playerRP) $ Circle (playerIndicatorSize playerRP)
    playerMarker = translate px py (Pictures [stunReadyCircle, playerIndicator])

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

    cellParams = cellRenderParameters rp
    mapGrid = Pictures $ concatMap (makeWallPictures cellParams) (Array.assocs (worldBoundaries world))

    makeWallPictures :: CellRenderParameters -> (Location, CellBoundaries) -> [Picture]
    makeWallPictures params ((x,y), CellBoundaries up right down left) =
      let coords = conversion (x,y)
          tl@(tlx, tly) = cellTopLeft coords
          tr@(trx, try) = cellTopRight coords
          bl@(blx, bly) = cellBottomLeft coords
          br@(brx, bry) = cellBottomRight coords
          width = cellWallWidth params
          stunBackground = if (x, y) `elem` stunCells world
            then Color (cellStunColor params) (Polygon [tl, tr, br, bl])
            else Blank
      in  [ stunBackground
          , drawEdge (tr, tl, (tlx, tly - width), (trx, try - width)) up
          , drawEdge (br, tr, (trx - width, try), (brx - width, bry)) right
          , drawEdge (bl, br, (brx, bry + width), (blx, bly + width)) down
          , drawEdge (tl, bl, (blx + width, bly), (tlx + width, tly)) left
          ]

    drawEdge :: (Point, Point, Point, Point) -> BoundaryType -> Picture
    drawEdge (p1, p2, _, _) (AdjacentCell _) = Line [p1, p2]
    drawEdge (p1, p2, p3, p4) _ = Color (cellWallColor cellParams) (Polygon [p1, p2, p3, p4])

    enemyParams = enemyRenderParameters rp
    enemyPic :: Enemy -> Picture
    enemyPic (Enemy loc _ _ currentStun) =
      let (centerX, centerY) = cellCenter $ conversion loc
          radius = (enemySize enemyParams) / 2.0
          tl = (centerX - radius, centerY + radius)
          tr = (centerX + radius, centerY + radius)
          br = (centerX + radius, centerY - radius)
          bl = (centerX - radius, centerY - radius)
          enemyColor = if currentStun == 0 then enemyBaseColor enemyParams else enemyStunnedColor enemyParams
      in  Color enemyColor (Polygon [tl, tr, br, bl])

inputHandler :: Event -> World -> World
inputHandler event w
  | worldResult w == GameWon = case event of
      (EventKey (SpecialKey KeyEnter) Down _ _) ->
        let (newMaze, gen') = generateRandomMaze (worldRandomGenerator w) (worldRows, worldCols)
            (newLocations, gen'') = runState
              (replicateM (length (worldEnemies w)) (generateRandomLocation (worldRows, worldCols)))
              gen'
        in  World (newPlayer playerParams) (0,0) (worldCols - 1, worldRows - 1) newMaze GameInProgress gen''
              (mkNewEnemy enemyParams <$> newLocations) [] 0 (worldParameters w)
      _ -> w
  | worldResult w == GameLost = case event of
      (EventKey (SpecialKey KeyEnter) Down _ _) ->
        let (newLocations, gen') = runState
              (replicateM (length (worldEnemies w)) (generateRandomLocation (worldRows, worldCols)))
              (worldRandomGenerator w)
        in  World (newPlayer playerParams) (0,0) (worldCols - 1, worldRows - 1) (worldBoundaries w) GameInProgress gen'
              (mkNewEnemy enemyParams <$> newLocations) [] 0 (worldParameters w)
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
          { worldPlayer = activatePlayerStun currentPlayer playerParams
          , worldEnemies = stunEnemyIfClose <$> worldEnemies w
          , stunCells = stunAffectedCells
          }
      _ -> w
  where
    playerParams = playerGameParameters . worldParameters $ w
    enemyParams = enemyGameParameters . worldParameters $ w

    worldRows = numRows . worldParameters $ w
    worldCols = numColumns . worldParameters $ w
    cellBounds = (worldBoundaries w) Array.! (playerLocation (worldPlayer w))
    currentPlayer = worldPlayer w

    nextLocation :: (CellBoundaries -> BoundaryType) -> Location
    nextLocation boundaryFunc = case boundaryFunc cellBounds of
      (AdjacentCell cell) -> cell
      _ -> playerLocation currentPlayer

    stunAffectedCells :: [Location]
    stunAffectedCells =
      let (cx, cy) = playerLocation currentPlayer
          r = stunRadius . playerGameParameters . worldParameters $ w
      in  [(x,y) | x <- [(cx - r)..(cx + r)], y <- [(cy - r)..(cy + r)], x >= 0 && x < worldCols, y >= 0 && y < worldRows]

    stunEnemyIfClose :: Enemy -> Enemy
    stunEnemyIfClose e = if enemyLocation e `elem` stunAffectedCells
      then stunEnemy e enemyParams
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
    randomMoveChance = enemyRandomMoveChance . enemyGameParameters . worldParameters $ w
    (newEnemies, newGen) = runState
      (sequence (updateEnemy (worldTime w) randomMoveChance (worldBoundaries w) (playerLocation player) <$> worldEnemies w))
      (worldRandomGenerator w)
    activeEnemyLocations = enemyLocation <$> filter (\e -> enemyCurrentStunTimer e == 0) (worldEnemies w)

-- Given a discrete location and some offsets, determine all the coordinates of the cell.
locationToCoords :: (Float, Float) -> Float -> Location -> CellCoordinates
locationToCoords (xOffset, yOffset) cellDimen (x, y) = CellCoordinates
  (centerX, centerY) -- Center
  (centerX - halfCell, centerY + halfCell) -- Top Left
  (centerX + halfCell, centerY + halfCell) -- Top Right
  (centerX - halfCell, centerY - halfCell) -- Bottom Left
  (centerX + halfCell, centerY - halfCell) -- Bottom Right
  where
    (centerX, centerY) = (xOffset + (fromIntegral x) * cellDimen, yOffset + (fromIntegral y) * cellDimen)
    halfCell = cellDimen / 2.0

updatePlayerOnTick :: Player -> Player
updatePlayerOnTick p = p { playerCurrentStunDelay = decrementIfPositive (playerCurrentStunDelay p)}

updateEnemy :: Word -> Word -> Maze -> Location -> Enemy -> State StdGen Enemy
updateEnemy time randomMoveChance maze playerLocation e@(Enemy location lagTime nextStun currentStun) =
  if not shouldUpdate
    then return $ e {enemyCurrentStunTimer = decrementIfPositive currentStun}
    else do
      gen <- get
      let (randomMoveRoll, gen') = randomR (1 :: Word, randomMoveChance) gen
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

mkNewEnemy :: EnemyGameParameters -> Location -> Enemy
mkNewEnemy params loc = Enemy loc (initialLagTime params) (initialStunTime params) 0

newPlayer :: PlayerGameParameters -> Player
newPlayer params = Player (0, 0) 0 (initialStunTimer params)

-- Mutators

incrementWorldTime :: World -> World
incrementWorldTime w = w { worldTime = worldTime w + 1 }

clearStunCells :: World -> World
clearStunCells w = w { stunCells = []}

activatePlayerStun :: Player -> PlayerGameParameters -> Player
activatePlayerStun (Player loc _ nextStunTimer) params =
  Player loc nextStunTimer newNextStun
  where
    newNextStun = min (stunTimerMax params) (nextStunTimer + (stunTimerIncrease params))

stunEnemy :: Enemy -> EnemyGameParameters -> Enemy
stunEnemy (Enemy loc lag nextStun _) params = Enemy loc newLag newNextStun nextStun
  where
    newNextStun = max (minStunTime params) (nextStun - (stunTimeDecrease params))
    newLag = max (minLagTime params) (lag - 1)

decrementIfPositive :: Word -> Word
decrementIfPositive 0 = 0
decrementIfPositive x = x - 1
