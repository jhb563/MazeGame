module MazeParser where

import Control.Monad (forM)
import Control.Monad.State (State, get, put, execState)
import Data.Char (toLower, intToDigit, toUpper)
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

import Runner (Location, CellBoundaries(..), BoundaryType(..), World, DirectionLens)

-- How do I want to store this? I see a couple options. One of these is to just have the wall layout. This
-- involves encoding each potential cell with a single character. I like this idea. The other alternative would be
-- to give a human readable format for the maze with letters for each enemy and that sort of thing. But that could
-- be derived later. What I actually want is a parser on the maze information and everything else we'll want in the
-- map. This means having different sections, and just one of those sections is the maze itself.
-- I mean realistically I need to have a token or something for each possible configuration. There are 16 of them.
-- So maybe a hexadecimal thing?

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

mazeParser :: (Int, Int) -> MParser (Map.Map Location CellBoundaries)
mazeParser (numRows, numColumns) = do
  rows <- forM [0..(numRows - 1)] $ \i -> do
    columns <- forM [0..(numColumns - 1)] $ \j -> do
      c <- M.hexDigitChar
      return (j, toLower c)
    M.newline
    return $ map (\(col, char) -> (((numRows - 1) - i, col), char))  columns
  return $ Map.fromList (cellSpecToBounds <$> (concat rows))
  where
    cellSpecToBounds :: (Location, Char) -> (Location, CellBoundaries)
    cellSpecToBounds (loc@(row, col), c) =
      let (topIsWall, rightIsWall, bottomIsWall, leftIsWall) = charToBoundsSet c
          topCell = if topIsWall then if row + 1 == numRows then WorldBoundary else Wall
                      else (AdjacentCell (row + 1, col))
          rightCell = if rightIsWall then if col + 1 == numColumns then WorldBoundary else Wall
                        else (AdjacentCell (row, col + 1))
          bottomCell = if bottomIsWall then if row == 0 then WorldBoundary else Wall
                         else (AdjacentCell (row - 1, col))
          leftCell = if leftIsWall then if col == 0 then WorldBoundary else Wall
                       else (AdjacentCell (row, col - 1))
      in  (loc, CellBoundaries topCell rightCell bottomCell leftCell)

charToBoundsSet :: Char -> (Bool, Bool, Bool, Bool)
charToBoundsSet '0' = (False, False, False, False)
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
charToBoundsSet _ = error "Invalid character!"

dumpMaze :: Map.Map Location CellBoundaries -> Text
dumpMaze maze = pack $ (unlines . reverse) (rowToString <$> cellsByRow)
  where
    cellsByRow :: [[(Location, CellBoundaries)]]
    cellsByRow = groupBy (\((r1, _), _) ((r2, _), _) -> r1 == r2) (Map.toList maze)

    rowToString :: [(Location, CellBoundaries)] -> String
    rowToString = map cellToChar

    cellToChar :: (Location, CellBoundaries) -> Char
    cellToChar (_, bounds) =
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

generateRandomMaze :: StdGen -> (Int, Int) -> Map.Map Location CellBoundaries
generateRandomMaze gen (numRows, numColumns) = currentBoundaries (execState dfsSearch initialState)
  where
    (startRow, g1) = randomR (0, numRows - 1) gen
    (startCol, g2) = randomR (0, numColumns - 1) g1
    initialState = SearchState g2 [(startRow, startCol)] initialBounds Set.empty

    initialBounds :: Map.Map Location CellBoundaries
    initialBounds = case M.runParser (mazeParser (numRows, numColumns)) "" fullString of
      Left _ -> error "Maze can't be parsed!"
      Right success -> success

    fullString :: Text
    fullString = pack . unlines $ take numRows $ repeat (take numColumns (repeat 'F'))

-- Pick out start location. Pick end location. Set up initial state. Run DFS

data SearchState = SearchState
  { randomGen :: StdGen
  , locationStack :: [Location]
  , currentBoundaries :: Map.Map Location CellBoundaries
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
    findCandidates :: Location -> Map.Map Location CellBoundaries -> Set.Set Location -> [(Location, CellBoundaries, Location, CellBoundaries)]
    findCandidates currentLocation@(row, col) bounds visited =
      let currentLocBounds = fromJust $ Map.lookup currentLocation bounds
          upLoc = (row + 1, col)
          maybeUpCell = case (upBoundary currentLocBounds, Set.member upLoc visited) of
                          (Wall, False) -> Just
                            ( upLoc
                            , (fromJust $ Map.lookup upLoc bounds) {downBoundary = AdjacentCell currentLocation}
                            , currentLocation
                            , currentLocBounds {upBoundary = AdjacentCell upLoc}
                            )
                          _ -> Nothing
          rightLoc = (row, col + 1)
          maybeRightCell = case (rightBoundary currentLocBounds, Set.member rightLoc visited) of
                             (Wall, False) -> Just
                               ( rightLoc
                               , (fromJust $ Map.lookup rightLoc bounds) {leftBoundary = AdjacentCell currentLocation}
                               , currentLocation
                               , currentLocBounds {rightBoundary = AdjacentCell rightLoc}
                               )
                             _ -> Nothing
          downLoc = (row - 1, col)
          maybeDownCell = case (downBoundary currentLocBounds, Set.member downLoc visited) of
                            (Wall, False) -> Just
                              ( downLoc
                              , (fromJust $ Map.lookup downLoc bounds) {upBoundary = AdjacentCell currentLocation}
                              , currentLocation
                              , currentLocBounds {downBoundary = AdjacentCell downLoc}
                              )
                            _ -> Nothing
          leftLoc = (row, col - 1)
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
