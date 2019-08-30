module Types where

import qualified Data.Array as Array
import qualified Data.Map as Map
import System.Random (StdGen)

import Graphics.Gloss

type Location = (Int, Int)

data CellCoordinates = CellCoordinates
  { cellCenter :: Point
  , cellTopLeft :: Point
  , cellTopRight :: Point
  , cellBottomLeft :: Point
  , cellBottomRight :: Point
  }

data BoundaryType = WorldBoundary | Wall Location | AdjacentCell Location
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
  , playerDrillsRemaining :: Word
  , playerLagTime :: Word
  }

data MoveDirection =
  DirectionUp |
  DirectionRight |
  DirectionDown |
  DirectionLeft |
  DirectionNone
  deriving (Eq)

data PlayerMove = PlayerMove
  { playerMoveDirection :: MoveDirection
  , activateStun :: Bool
  , drillDirection :: MoveDirection
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
  , worldDrillPowerUpLocations :: [Location]
  , stunCells :: [Location]
  , worldTime :: Word
  , worldParameters :: GameParameters
  , worldGridNums :: [Float]
  }

data GameParameters = GameParameters
  { numRows :: Int
  , numColumns :: Int
  , numEnemies :: Int
  , numDrillPowerups :: Int
  , tickRate :: Int
  , playerGameParameters :: PlayerGameParameters
  , enemyGameParameters :: EnemyGameParameters
  , randomGeneratorSeed :: Maybe Int
  , usePlayerAI :: Bool
  }

data PlayerGameParameters = PlayerGameParameters
  { initialStunTimer :: Word
  , stunTimerIncrease :: Word
  , stunTimerMax :: Word
  , stunRadius :: Int
  , initialDrills :: Word
  , lagTime :: Word
  }

data EnemyGameParameters = EnemyGameParameters
  { initialStunTime :: Word
  , stunTimeDecrease :: Word
  , minStunTime :: Word
  , enemyRandomMoveChance :: Word
  , initialLagTime :: Word
  , minLagTime :: Word
  }

defaultGameParameters :: GameParameters
defaultGameParameters = GameParameters
  10 10 5 0 20 playerParams enemyParams Nothing False
  where
    playerParams = PlayerGameParameters
      200 10 (maxBound :: Word) 2 1 5
    enemyParams = EnemyGameParameters 60 5 20 5 20 10

data RenderParameters = RenderParameters
  { screenDimen :: Int
  , screenOffsetX :: Int
  , screenOffsetY :: Int
  , textOffset :: (Float, Float)
  , textScale :: (Float, Float)
  , playerRenderParameters :: PlayerRenderParameters
  , enemyRenderParameters :: EnemyRenderParameters
  , cellRenderParameters :: CellRenderParameters
  }

data PlayerRenderParameters = PlayerRenderParameters
  { playerIndicatorSize :: Float
  , playerIndicatorColor :: Color
  , playerStunIndicatorSize :: Float
  , playerStunIndicatorColor :: Color
  , playerDrillPowerupSize :: Float
  , playerDrillIndicatorSize :: Float
  , playerDrillColor :: Color
  }

data EnemyRenderParameters = EnemyRenderParameters
  { enemySize :: Float
  , enemyBaseColor :: Color
  , enemyStunnedColor :: Color
  }

data CellRenderParameters = CellRenderParameters
  { cellWallColor :: Color
  , cellStunColor :: Color
  , cellWallWidth :: Float
  }

defaultRenderParameters :: RenderParameters
defaultRenderParameters = RenderParameters
  625 10 10 (-275, 0) (0.12, 0.25) playerParams enemyParams cellParams
  where
    playerParams = PlayerRenderParameters 10 black 5 red 5.0 2.0 violet
    enemyParams = EnemyRenderParameters 10 orange yellow
    cellParams = CellRenderParameters blue cyan 2
