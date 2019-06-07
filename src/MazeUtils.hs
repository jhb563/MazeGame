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

getShortestPathWithDrills :: Maze -> Word -> Set.Set Location -> Location -> Location -> [Location]
getShortestPathWithDrills maze initialNumDrills initialDrillLocs initialLocation targetLocation = evalState
  (drillBFS maze targetLocation)
  (DrillBFSState (Seq.singleton initialItem) (Set.singleton initialItem) Map.empty)
  where
    initialItem = (initialLocation, initialNumDrills, initialDrillLocs)

type DrillSearchItem = (Location, Word, Set.Set Location)

data DrillBFSState = DrillBFSState
  (Seq.Seq DrillSearchItem)
  (Set.Set DrillSearchItem)
  (Map.Map DrillSearchItem DrillSearchItem)

drillBFS :: Maze -> Location -> State DrillBFSState [Location]
drillBFS maze targetLocation = do
  DrillBFSState searchQueue visitedSet parentsMap <- get
  if Seq.null searchQueue
    then return []
    else do
      let item@(nextLoc, drillsRemaining, drillLocs) = Seq.index searchQueue 0
      if nextLoc == targetLocation
        then return $ (\(l, _, _) -> l) <$> (unwindPath parentsMap [item])
        else do
          let nextItems = getDrillAdjacentItems maze item
              unvisitedNextItems = filter (\i -> not (Set.member i visitedSet)) nextItems
              newSearchQueue = foldr (flip (Seq.|>)) (Seq.drop 1 searchQueue) unvisitedNextItems
              newVisitedSet = Set.insert item visitedSet
              newParentsMap = foldr (\i -> Map.insert i item) parentsMap unvisitedNextItems
          put (DrillBFSState newSearchQueue newVisitedSet newParentsMap)
          drillBFS maze targetLocation
  where
    unwindPath parentsMap currentPath = case Map.lookup (head currentPath) parentsMap of
      Nothing -> tail currentPath
      Just parent -> unwindPath parentsMap (parent : currentPath)

getDrillAdjacentItems :: Maze -> DrillSearchItem -> [DrillSearchItem]
getDrillAdjacentItems maze (location, drillsRemaining, drillPowerups) =
  mkItemFromResult <$> (catMaybes [maybeUpLoc, maybeRightLoc, maybeDownLoc, maybeLeftLoc])
  where
    bounds = maze Array.! location
    canDrill = drillsRemaining > 0
    mkItemFromResult (loc, usedDrill) =
      let drillsAfterMove = if usedDrill then drillsRemaining - 1 else drillsRemaining
          (drillsAfterPickup, newDrillLocs) = if Set.member loc drillPowerups
                                                then (drillsAfterMove + 1, Set.delete loc drillPowerups)
                                                else (drillsAfterMove, drillPowerups)
      in  (loc, drillsAfterPickup, newDrillLocs)
    maybeUpLoc = case upBoundary bounds of
      (AdjacentCell loc) -> Just (loc, False)
      (Wall loc) -> if canDrill then Just (loc, True) else Nothing
      _ -> Nothing
    maybeRightLoc = case rightBoundary bounds of
      (AdjacentCell loc) -> Just (loc, False)
      (Wall loc) -> if canDrill then Just (loc, True) else Nothing
      _ -> Nothing
    maybeDownLoc = case downBoundary bounds of
      (AdjacentCell loc) -> Just (loc, False)
      (Wall loc) -> if canDrill then Just (loc, True) else Nothing
      _ -> Nothing
    maybeLeftLoc = case leftBoundary bounds of
      (AdjacentCell loc) -> Just (loc, False)
      (Wall loc) -> if canDrill then Just (loc, True) else Nothing
      _ -> Nothing
