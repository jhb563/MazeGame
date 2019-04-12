module MazeUtils where

import Control.Monad.State
import qualified Data.Array as Array
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Types

getShortestPath :: Maze -> Location -> Location -> [Location]
getShortestPath maze initialLocation targetLocation = evalState
  (bfs maze initialLocation targetLocation)
  (BFSState (Seq.singleton initialLocation) (Set.singleton initialLocation) Map.empty)

data BFSState = BFSState
  { bfsSearchQueue :: Seq.Seq Location
  , bfsVisistedLocations :: Set.Set Location
  , bfsParents :: Map.Map Location Location
  }

bfs :: Maze -> Location -> Location -> State BFSState [Location]
bfs maze initialLocation targetLocation = do
  BFSState searchQueue visitedSet parentsMap <- get
  if Seq.null searchQueue
    then return []
    else do
      let nextLoc = Seq.index searchQueue 0
      if nextLoc == targetLocation
        then return (unwindPath parentsMap [targetLocation])
        else do
          let adjacentCells = getAdjacentLocations maze nextLoc
              unvisitedNextCells = filter (\l -> not (Set.member l visitedSet)) adjacentCells
              newSearchQueue = foldr (flip (Seq.|>)) (Seq.drop 1 searchQueue) unvisitedNextCells
              newVisitedSet = Set.insert nextLoc visitedSet
              newParentsMap = foldr (\l -> Map.insert l nextLoc) parentsMap unvisitedNextCells
          put (BFSState newSearchQueue newVisitedSet newParentsMap)
          bfs maze initialLocation targetLocation
  where
    unwindPath parentsMap currentPath = case Map.lookup (head currentPath) parentsMap of
      Nothing -> tail currentPath
      Just parent -> unwindPath parentsMap (parent : currentPath)

getAdjacentLocations :: Maze -> Location -> [Location]
getAdjacentLocations maze location = catMaybes [maybeUpLoc, maybeRightLoc, maybeDownLoc, maybeLeftLoc]
  where
    bounds = maze Array.! location
    maybeUpLoc = case upBoundary bounds of
      (AdjacentCell loc) -> Just loc
      _ -> Nothing
    maybeRightLoc = case rightBoundary bounds of
      (AdjacentCell loc) -> Just loc
      _ -> Nothing
    maybeDownLoc = case downBoundary bounds of
      (AdjacentCell loc) -> Just loc
      _ -> Nothing
    maybeLeftLoc = case leftBoundary bounds of
      (AdjacentCell loc) -> Just loc
      _ -> Nothing

