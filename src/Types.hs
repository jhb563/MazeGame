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

type Maze = Array.Array Location CellBoundaries

data GameResult = GameInProgress | GameWon | GameLost
  deriving (Show, Eq)

data Player = Player
  { playerLocation :: Location
  , playerCurrentStunDelay :: Word
  , playerNextStunDelay :: Word
  }

data Enemy = Enemy
  { enemyLocation :: Location
  , enemyLagTime :: Word
  , enemyNextStunDuration :: Word
  , enemyCurrentStunTimer :: Word
  }

data World = World
  { worldPlayer :: Player
  , startLocation :: Location
  , endLocation :: Location
  , worldBoundaries :: Maze
  , worldResult :: GameResult
  , worldRandomGenerator :: StdGen
  , worldEnemies :: [Enemy]
  , stunCells :: [Location]
  , worldTime :: Word
  }
