import qualified Data.Map as Map
import Data.Text (Text, pack)

import Text.Megaparsec

import Test.Hspec

import MazeParser
import Types

main :: IO ()
main = hspec $ do
  mazeParsingSpec
  dumpMazeSpec

shouldMatch :: (Show a, Eq a) => MParser a -> Text -> a -> IO ()
shouldMatch parser input result = runParser parser "" input `shouldBe` (Right result)

mazeParsingSpec :: Spec
mazeParsingSpec = describe "Parse a maze" $ do
  it "Should match our expected maze" $
    shouldMatch (mazeParser (5,5)) testMazeString testSolution

dumpMazeSpec :: Spec
dumpMazeSpec = describe "Dump Maze to Text" $ do
  it "Should match our expected text output" $
    dumpMaze testSolution `shouldBe` testMazeString

testMazeString :: Text
testMazeString = pack $ unlines
  [ "98CDF"
  , "1041C"
  , "34775"
  , "90AA4"
  , "32EB6"
  ]

testSolution :: Map.Map Location CellBoundaries
testSolution = Map.fromList
  [ ((0,0), CellBoundaries (AdjacentCell (0,1)) (AdjacentCell (1,0)) WorldBoundary WorldBoundary)
  , ((1,0), CellBoundaries (AdjacentCell (1,1)) (AdjacentCell (2,0)) WorldBoundary (AdjacentCell (0,0)))
  , ((2,0), CellBoundaries Wall Wall WorldBoundary (AdjacentCell (1,0)))
  , ((3,0), CellBoundaries Wall (AdjacentCell (4,0)) WorldBoundary Wall)
  , ((4,0), CellBoundaries (AdjacentCell (4,1)) WorldBoundary WorldBoundary (AdjacentCell (3,0)))
  , ((0,1), CellBoundaries Wall (AdjacentCell (1,1)) (AdjacentCell (0,0)) WorldBoundary)
  , ((1,1), CellBoundaries (AdjacentCell (1,2)) (AdjacentCell (2,1)) (AdjacentCell (1,0)) (AdjacentCell (0,1)))
  , ((2,1), CellBoundaries Wall (AdjacentCell (3,1)) Wall (AdjacentCell (1,1)))
  , ((3,1), CellBoundaries Wall (AdjacentCell (4,1)) Wall (AdjacentCell (2,1)))
  , ((4,1), CellBoundaries (AdjacentCell (4,2)) WorldBoundary (AdjacentCell (4,0)) (AdjacentCell (3,1)))
  , ((0,2), CellBoundaries (AdjacentCell (0,3)) (AdjacentCell (1,2)) Wall WorldBoundary)
  , ((1,2), CellBoundaries (AdjacentCell (1,3)) Wall (AdjacentCell (1,1)) (AdjacentCell (0,2)))
  , ((2,2), CellBoundaries (AdjacentCell (2,3)) Wall Wall Wall)
  , ((3,2), CellBoundaries (AdjacentCell (3,3)) Wall Wall Wall)
  , ((4,2), CellBoundaries (AdjacentCell (4,3)) WorldBoundary (AdjacentCell (4,1)) Wall)
  , ((0,3), CellBoundaries (AdjacentCell (0,4)) (AdjacentCell (1,3)) (AdjacentCell (0,2)) WorldBoundary)
  , ((1,3), CellBoundaries (AdjacentCell (1,4)) (AdjacentCell (2,3)) (AdjacentCell (1,2)) (AdjacentCell (0,3)))
  , ((2,3), CellBoundaries (AdjacentCell (2,4)) Wall (AdjacentCell (2,2)) (AdjacentCell (1,3)))
  , ((3,3), CellBoundaries (AdjacentCell (3,4)) (AdjacentCell (4,3)) (AdjacentCell (3,2)) Wall)
  , ((4,3), CellBoundaries Wall WorldBoundary (AdjacentCell (4,2)) (AdjacentCell (3,3)))
  , ((0,4), CellBoundaries WorldBoundary (AdjacentCell (1,4)) (AdjacentCell (0,3)) WorldBoundary)
  , ((1,4), CellBoundaries WorldBoundary (AdjacentCell (2,4)) (AdjacentCell (1,3)) (AdjacentCell (0,4)))
  , ((2,4), CellBoundaries WorldBoundary Wall (AdjacentCell (2,3)) (AdjacentCell (1,4)))
  , ((3,4), CellBoundaries WorldBoundary Wall (AdjacentCell (3,3)) Wall)
  , ((4,4), CellBoundaries WorldBoundary WorldBoundary Wall Wall)
  ]
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
-- Test Case
-- 98CDF
-- 1041C
-- 34775
-- 90AA4
-- 32EB6
