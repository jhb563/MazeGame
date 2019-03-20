module Runner where

import Data.Ix (range)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

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

boundariesMap :: (Int, Int) -> Map.Map Location CellBoundaries
boundariesMap (numColumns, numRows) = Map.fromList (buildBounds <$> (range ((0,0), (numColumns, numRows))))
  where
    buildBounds :: Location -> (Location, CellBoundaries)
    buildBounds loc = (loc, simpleBoundaries (numColumns, numRows) loc)

simpleBoundaries :: (Int, Int) -> Location -> CellBoundaries
simpleBoundaries (numColumns, numRows) (x, y) = CellBoundaries
  (if y + 1 < numRows then AdjacentCell (x, y+1) else WorldBoundary)
  (if x + 1 < numColumns then AdjacentCell (x+1, y) else WorldBoundary)
  (if y > 0 then AdjacentCell (x, y-1) else WorldBoundary)
  (if x > 0 then AdjacentCell (x-1, y) else WorldBoundary)

type Location = (Int, Int)

data CellCoordinates = CellCoordinates
  { cellCenter :: Point
  , cellTopLeft :: Point
  , cellTopRight :: Point
  , cellBottomLeft :: Point
  , cellBottomRight :: Point
  }

data BoundaryType = WorldBoundary | Wall | AdjacentCell Location
  deriving (Show, Eq)
data CellBoundaries = CellBoundaries
  { upBoundary :: BoundaryType
  , rightBoundary :: BoundaryType
  , downBoundary :: BoundaryType
  , leftBoundary :: BoundaryType
  }
  deriving (Show, Eq)

type DirectionLens = CellBoundaries -> BoundaryType

data World = World
  { playerLocation :: Location
  , startLocation :: Location
  , endLocation :: Location
  , worldBoundaries :: Map.Map Location CellBoundaries
  }

main :: IO ()
main = play
  windowDisplay
  white
  20
  (World (0, 0) (0,0) (24,24) (boundariesMap (25, 25)))
  (drawingFunc (globalXOffset, globalYOffset) globalCellSize)
  inputHandler
  updateFunc

-- First argument is offset from true 0,0 to the center of the grid space 0,0
drawingFunc :: (Float, Float) -> Float -> World -> Picture
drawingFunc (xOffset, yOffset) cellSize world = Pictures [mapGrid, startPic, endPic, playerMarker]
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

    mapGrid = Pictures $ concatMap makeWallPictures (Map.toList (worldBoundaries world))

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

inputHandler :: Event -> World -> World
inputHandler event w = case event of
  (EventKey (SpecialKey KeyUp) Down _ _) -> w { playerLocation = nextLocation upBoundary }
  (EventKey (SpecialKey KeyDown) Down _ _) -> w { playerLocation = nextLocation downBoundary }
  (EventKey (SpecialKey KeyRight) Down _ _) -> w { playerLocation = nextLocation rightBoundary }
  (EventKey (SpecialKey KeyLeft) Down _ _) -> w { playerLocation = nextLocation leftBoundary }
  _ -> w
  where
    cellBounds = fromJust $ Map.lookup (playerLocation w) (worldBoundaries w)

    nextLocation :: (CellBoundaries -> BoundaryType) -> Location
    nextLocation boundaryFunc = case boundaryFunc cellBounds of
      (AdjacentCell cell) -> cell
      _ -> playerLocation w

updateFunc :: Float -> World -> World
updateFunc _ = id

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

{-simulate 
  (InWindow "Nice Window" (200, 200) (50, 50))
  white 20
  (0, 0)
  (\(theta, dtheta) -> Line [(0, 0), (50 * cos theta, 50 * sin theta)])
  (\_ dt (theta, dtheta) -> (theta + dt * dtheta, dtheta - dt * (cos theta)))-}
