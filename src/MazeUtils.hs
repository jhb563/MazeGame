{-# LANGUAGE TupleSections #-}

module MazeUtils where

import Control.Monad.State
import qualified Data.Array as Array
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Types

getShortestPathToTargetsWithLimit :: Maze -> Location -> Set.Set Location -> Maybe Int -> [Location]
getShortestPathToTargetsWithLimit maze initialLocation targetLocations maxRange = evalState
  (bfs maze initialLocation targetLocations maxRange)
  (BFSState (Seq.singleton (initialLocation, 0)) (Set.singleton initialLocation) Map.empty)

getShortestPath :: Maze -> Location -> Location -> [Location]
getShortestPath maze initialLocation targetLocation = getShortestPathToTargetsWithLimit
  maze initialLocation (Set.singleton targetLocation) Nothing

data BFSState = BFSState
  { bfsSearchQueue :: Seq.Seq (Location, Int)
  , bfsVisistedLocations :: Set.Set Location
  , bfsParents :: Map.Map Location Location
  }

bfs :: Maze -> Location -> Set.Set Location -> Maybe Int -> State BFSState [Location]
bfs maze initialLocation targetLocations maxRange = do
  BFSState searchQueue visitedSet parentsMap <- get
  if Seq.null searchQueue
    then return []
    else do
      let (nextLoc, distance) = Seq.index searchQueue 0
      if Set.member nextLoc targetLocations
        then return (unwindPath parentsMap [nextLoc])
        else do
          let adjacentCells = (, distance + 1) <$> getAdjacentLocations maze nextLoc
              unvisitedNextCells = filter (shouldAddNextCell visitedSet) adjacentCells
              newSearchQueue = foldr (flip (Seq.|>)) (Seq.drop 1 searchQueue) unvisitedNextCells
              newVisitedSet = Set.insert nextLoc visitedSet
              newParentsMap = foldr (\(l, _) -> Map.insert l nextLoc) parentsMap unvisitedNextCells
          put (BFSState newSearchQueue newVisitedSet newParentsMap)
          bfs maze initialLocation targetLocations maxRange
  where
    shouldAddNextCell visitedSet (loc, distance) = case maxRange of
      Nothing -> not (Set.member loc visitedSet)
      Just x -> distance <= x && not (Set.member loc visitedSet)
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
