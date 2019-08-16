module WorldParser where

import Data.Aeson
import Data.List (intercalate)
import qualified Data.Vector as V
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (getCurrentDirectory)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random (getStdGen, mkStdGen)

import JSONInstances ()
import Types

loadWorldFromFile :: FilePath -> IO World
loadWorldFromFile fp = do
  parseResult <- Data.Aeson.decodeFileStrict' fp
  case parseResult of
    Just w -> do
      gen <- case randomGeneratorSeed (worldParameters w) of
        Nothing -> getStdGen
        Just i -> return $ mkStdGen i
      return $ w { worldRandomGenerator = gen }
    Nothing -> error $ "Couldn't parse world from file " ++ fp ++ "!"

unsafeSaveWorldToFile :: World -> World
unsafeSaveWorldToFile w = unsafePerformIO $ do
  timeAsString <- show . floor <$> getPOSIXTime
  currentDir <- getCurrentDirectory
  let filename = currentDir ++ "/maze_game_save_" ++ timeAsString
  encodeFile filename w
  return w

unsafeSaveMove :: Int -> World -> World -> World
unsafeSaveMove moveChoice prevWorld nextWorld = unsafePerformIO $ do
  handle <- openFile "moves.csv" AppendMode
  let numbers = show <$> (V.toList (vectorizeWorld prevWorld) ++ ([fromIntegral moveChoice]))
  let csvString = intercalate "," numbers
  hPutStrLn handle csvString
  hClose handle
  return nextWorld
