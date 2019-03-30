module MazeParser where

import Control.Monad (forM)
import Control.Monad.State (State, get, put, execState)
import qualified Data.Array as Array
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
  return $ Map.fromList (cellSpecToBounds <$> (concat rows))
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
    transposedMap = Map.mapKeys (\(x, y) -> (y, x)) maze

    cellsByRow :: [[(Location, CellBoundaries)]]
    cellsByRow = groupBy (\((r1, _), _) ((r2, _), _) -> r1 == r2) (Map.toList transposedMap)

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

generateRandomMaze :: StdGen -> (Int, Int) -> Maze
generateRandomMaze gen (numRows, numColumns) = currentBoundaries (execState dfsSearch initialState)
  where
    (startX, g1) = randomR (0, numColumns - 1) gen
    (startY, g2) = randomR (0, numRows - 1) g1
    initialState = SearchState g2 [(startX, startY)] initialBounds Set.empty

    initialBounds :: Maze
    initialBounds = case M.runParser (mazeParser (numRows, numColumns)) "" fullString of
      Left _ -> error "Maze can't be parsed!"
      Right success -> success

    fullString :: Text
    fullString = pack . unlines $ take numRows $ repeat (take numColumns (repeat 'F'))

-- Pick out start location. Pick end location. Set up initial state. Run DFS

data SearchState = SearchState
  { randomGen :: StdGen
  , locationStack :: [Location]
  , currentBoundaries :: Maze
  , visitedCells :: Set.Set Location
  }

dfsSearch :: State SearchState ()
dfsSearch = do
  (SearchState gen locs bounds visited) <- get
  case locs of
    [] -> return ()
    (currentLoc : rest) -> do
      let candidateLocs = findCandidates currentLoc bounds visited
      if null candidateLocs
        then put (SearchState gen rest bounds visited) >> dfsSearch
        else chooseCandidate candidateLocs >> dfsSearch

  where
    findCandidates :: Location -> Maze -> Set.Set Location -> [(Location, CellBoundaries, Location, CellBoundaries)]
    findCandidates currentLocation@(x, y) bounds visited =
      let currentLocBounds = fromJust $ Map.lookup currentLocation bounds
          upLoc = (x, y + 1)
          maybeUpCell = case (upBoundary currentLocBounds, Set.member upLoc visited) of
                          (Wall, False) -> Just
                            ( upLoc
                            , (fromJust $ Map.lookup upLoc bounds) {downBoundary = AdjacentCell currentLocation}
                            , currentLocation
                            , currentLocBounds {upBoundary = AdjacentCell upLoc}
                            )
                          _ -> Nothing
          rightLoc = (x + 1, y)
          maybeRightCell = case (rightBoundary currentLocBounds, Set.member rightLoc visited) of
                             (Wall, False) -> Just
                               ( rightLoc
                               , (fromJust $ Map.lookup rightLoc bounds) {leftBoundary = AdjacentCell currentLocation}
                               , currentLocation
                               , currentLocBounds {rightBoundary = AdjacentCell rightLoc}
                               )
                             _ -> Nothing
          downLoc = (x, y - 1)
          maybeDownCell = case (downBoundary currentLocBounds, Set.member downLoc visited) of
                            (Wall, False) -> Just
                              ( downLoc
                              , (fromJust $ Map.lookup downLoc bounds) {upBoundary = AdjacentCell currentLocation}
                              , currentLocation
                              , currentLocBounds {downBoundary = AdjacentCell downLoc}
                              )
                            _ -> Nothing
          leftLoc = (x - 1, y)
          maybeLeftCell = case (leftBoundary currentLocBounds, Set.member leftLoc visited) of
                            (Wall, False) -> Just
                              ( leftLoc
                              , (fromJust $ Map.lookup leftLoc bounds) {rightBoundary = AdjacentCell currentLocation}
                              , currentLocation
                              , currentLocBounds {leftBoundary = AdjacentCell leftLoc}
                              )
                            _ -> Nothing
      in  catMaybes [maybeUpCell, maybeRightCell, maybeDownCell, maybeLeftCell]


    -- Input must be non empty!
    chooseCandidate :: [(Location, CellBoundaries, Location, CellBoundaries)] -> State SearchState ()
    chooseCandidate candidates = do
      (SearchState gen currentLocs boundsMap visited) <- get
      let (randomIndex, newGen) = randomR (0, (length candidates) - 1) gen
          (chosenLocation, newChosenBounds, prevLocation, newPrevBounds) = candidates !! randomIndex
          newBounds = Map.insert prevLocation newPrevBounds (Map.insert chosenLocation newChosenBounds boundsMap)
          newVisited = Set.insert chosenLocation visited
      put (SearchState newGen (chosenLocation : currentLocs) newBounds newVisited)
