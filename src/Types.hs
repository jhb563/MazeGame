module Types where

import qualified Data.Array as Array
import qualified Data.Map as Map
import System.Random (StdGen)

import Graphics.Gloss (Point)

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

type Maze = Map.Map Location CellBoundaries

data GameResult = GameInProgress | GameWon
  deriving (Show, Eq)

data World = World
  { playerLocation :: Location
  , startLocation :: Location
  , endLocation :: Location
  , worldBoundaries :: Maze
  , worldResult :: GameResult
  , worldRandomGenerator :: StdGen
  }
