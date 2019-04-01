module MazeParser where

import Control.Monad (forM)
import Control.Monad.State (State, get, put, execState, StateT, execStateT, lift)
import qualified Data.Array as Array
import qualified Data.Array.IO as IA
import Data.Char (toLower, intToDigit, toUpper, digitToInt)
import Data.Either (fromRight)
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void (Void)
import System.Random (StdGen, randomR)

import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

import Types (Location, CellBoundaries(..), BoundaryType(..), World, Maze)

-- top-right-bottom-left
-- 0 = 0000
-- 1 = 0001
-- 2 = 0010
-- 3 = 0011
-- 4 = 0100
-- 5 = 0101
-- 6 = 0110
-- 7 = 0111
-- 8 = 1000
-- 9 = 1001
-- A = 1010
-- B = 1011
-- C = 1100
-- D = 1101
-- E = 1110
-- F = 1111

type MParser = Parsec Void Text

parseWorldFromFile :: FilePath -> IO World
parseWorldFromFile = undefined

sampleMaze :: Maze
sampleMaze = fromRight undefined $ M.runParser (mazeParser (5,5)) "" testMaze

testMaze :: Text
testMaze = pack $ unlines
  [ "98CDF"
  , "1041C"
  , "34775"
  , "90AA4"
  , "32EB6"
  ]

mazeParser :: (Int, Int) -> MParser Maze
mazeParser (numRows, numColumns) = do
  rows <- forM [(numRows - 1),(numRows - 2)..0] $ \i -> do
    columns <- forM [0..(numColumns - 1)] $ \j -> do
      c <- M.hexDigitChar
      return (j, c)
    M.newline
    return $ map (\(col, char) -> ((col, i), char))  columns
  return $ Array.array ((0,0), (numColumns - 1, numRows - 1)) (cellSpecToBounds <$> (concat rows))
  where
    cellSpecToBounds :: (Location, Char) -> (Location, CellBoundaries)
    cellSpecToBounds (loc@(x, y), c) =
      let (topIsWall, rightIsWall, bottomIsWall, leftIsWall) = charToBoundsSet c
          topCell = if topIsWall then if y + 1 == numRows then WorldBoundary else Wall
                      else (AdjacentCell (x, y + 1))
          rightCell = if rightIsWall then if x + 1 == numColumns then WorldBoundary else Wall
                        else (AdjacentCell (x + 1, y))
          bottomCell = if bottomIsWall then if y == 0 then WorldBoundary else Wall
                         else (AdjacentCell (x, y - 1))
          leftCell = if leftIsWall then if x == 0 then WorldBoundary else Wall
                       else (AdjacentCell (x - 1, y))
      in  (loc, CellBoundaries topCell rightCell bottomCell leftCell)

charToBoundsSet :: Char -> (Bool, Bool, Bool, Bool)
charToBoundsSet c =
  ( num > 7
  , num `mod` 8 > 3
  , num `mod` 4 > 1
  , num `mod` 2 == 1
  )
  where
    num = digitToInt c
{-charToBoundsSet '0' = (False, False, False, False)
charToBoundsSet '1' = (False, False, False, True)
charToBoundsSet '2' = (False, False, True, False)
charToBoundsSet '3' = (False, False, True, True)
charToBoundsSet '4' = (False, True, False, False)
charToBoundsSet '5' = (False, True, False, True)
charToBoundsSet '6' = (False, True, True, False)
charToBoundsSet '7' = (False, True, True, True)
charToBoundsSet '8' = (True, False, False, False)
charToBoundsSet '9' = (True, False, False, True)
charToBoundsSet 'a' = (True, False, True, False)
charToBoundsSet 'b' = (True, False, True, True)
charToBoundsSet 'c' = (True, True, False, False)
charToBoundsSet 'd' = (True, True, False, True)
charToBoundsSet 'e' = (True, True, True, False)
charToBoundsSet 'f' = (True, True, True, True)
charToBoundsSet _ = error "Invalid character!"-}

dumpMaze :: Maze -> Text
dumpMaze maze = pack $ (unlines . reverse) (rowToString <$> cellsByRow)
  where
    transposedMap :: Maze
    transposedMap = Array.ixmap (Array.bounds maze) (\(x, y) -> (y, x)) maze

    cellsByRow :: [[(Location, CellBoundaries)]]
    cellsByRow = groupBy (\((r1, _), _) ((r2, _), _) -> r1 == r2) (Array.assocs transposedMap)

    rowToString :: [(Location, CellBoundaries)] -> String
    rowToString = map (cellToChar . snd)

    cellToChar :: CellBoundaries -> Char
    cellToChar bounds =
      let top = case upBoundary bounds of
                  (AdjacentCell _) -> 0
                  _ -> 8
          right = case rightBoundary bounds of
                  (AdjacentCell _) -> 0
                  _ -> 4
          down = case downBoundary bounds of
                  (AdjacentCell _) -> 0
                  _ -> 2
          left = case leftBoundary bounds of
                  (AdjacentCell _) -> 0
                  _ -> 1
      in  toUpper $ intToDigit (top + right + down + left)

generateRandomMaze :: StdGen -> (Int, Int) -> IO (Maze, StdGen)
generateRandomMaze gen (numRows, numColumns) = do
  initialMutableBounds <- IA.thaw initialBounds
  let initialState = SearchState g2 [(startX, startY)] initialMutableBounds Set.empty
  finalState <- execStateT dfsSearch initialState
  finalBounds <- IA.freeze (currentBoundaries finalState)
  return (finalBounds, randomGen finalState)
  where
    (startX, g1) = randomR (0, numColumns - 1) gen
    (startY, g2) = randomR (0, numRows - 1) g1

    initialBounds :: Maze
    initialBounds = case M.runParser (mazeParser (numRows, numColumns)) "" fullString of
      Left _ -> error "Maze can't be parsed!"
      Right success -> success

    fullString :: Text
    fullString = pack . unlines $ take numRows $ repeat (take numColumns (repeat 'F'))

-- Pick out start location. Pick end location. Set up initial state. Run DFS

type MMaze = IA.IOArray Location CellBoundaries

data SearchState = SearchState
  { randomGen :: StdGen
  , locationStack :: [Location]
  , currentBoundaries :: MMaze
  , visitedCells :: Set.Set Location
  }

dfsSearch :: StateT SearchState IO ()
dfsSearch = do
  (SearchState gen locs bounds visited) <- get
  case locs of
    [] -> return ()
    (currentLoc : rest) -> do
      candidateLocs <- lift $ findCandidates currentLoc bounds visited
      if null candidateLocs
        then put (SearchState gen rest bounds visited) >> dfsSearch
        else chooseCandidate candidateLocs >> dfsSearch

findCandidates :: Location -> MMaze -> Set.Set Location -> IO [(Location, CellBoundaries, Location, CellBoundaries)]
findCandidates currentLocation@(x, y) bounds visited = do
  currentLocBounds <- IA.readArray bounds currentLocation
  let upLoc = (x, y + 1)
      rightLoc = (x + 1, y)
      downLoc = (x, y - 1)
      leftLoc = (x - 1, y)
  maybeUpCell <- case (upBoundary currentLocBounds, Set.member upLoc visited) of
    (Wall, False) -> do
      upBounds <- IA.readArray bounds upLoc
      return $ Just
        ( upLoc
        , upBounds {downBoundary = AdjacentCell currentLocation}
        , currentLocation
        , currentLocBounds {upBoundary = AdjacentCell upLoc}
        )
    _ -> return Nothing
  maybeRightCell <- case (rightBoundary currentLocBounds, Set.member rightLoc visited) of
    (Wall, False) -> do
      rightBounds <- IA.readArray bounds rightLoc
      return $ Just
        ( rightLoc
        , rightBounds {leftBoundary = AdjacentCell currentLocation}
        , currentLocation
        , currentLocBounds {rightBoundary = AdjacentCell rightLoc}
        )
    _ -> return Nothing
  maybeDownCell <- case (downBoundary currentLocBounds, Set.member downLoc visited) of
    (Wall, False) -> do
      downBounds <- IA.readArray bounds downLoc
      return $ Just
        ( downLoc
        , downBounds {upBoundary = AdjacentCell currentLocation}
        , currentLocation
        , currentLocBounds {downBoundary = AdjacentCell downLoc}
        )
    _ -> return Nothing
  maybeLeftCell <- case (leftBoundary currentLocBounds, Set.member leftLoc visited) of
    (Wall, False) -> do
      leftBounds <- IA.readArray bounds leftLoc
      return $ Just
        ( leftLoc
        , leftBounds {rightBoundary = AdjacentCell currentLocation}
        , currentLocation
        , currentLocBounds {leftBoundary = AdjacentCell leftLoc}
        )
    _ -> return Nothing
  return $ catMaybes [maybeUpCell, maybeRightCell, maybeDownCell, maybeLeftCell]

-- Input must be non empty!
chooseCandidate :: [(Location, CellBoundaries, Location, CellBoundaries)] -> StateT SearchState IO ()
chooseCandidate candidates = do
  (SearchState gen currentLocs boundsMap visited) <- get
  let (randomIndex, newGen) = randomR (0, (length candidates) - 1) gen
      (chosenLocation, newChosenBounds, prevLocation, newPrevBounds) = candidates !! randomIndex
      newVisited = Set.insert chosenLocation visited
  lift $ IA.writeArray boundsMap chosenLocation newChosenBounds
  lift $ IA.writeArray boundsMap prevLocation newPrevBounds
  put (SearchState newGen (chosenLocation : currentLocs) boundsMap newVisited)
