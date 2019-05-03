module WorldParser where

import Data.Aeson
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)

import JSONInstances ()
import Types

loadWorldFromFile :: FilePath -> IO World
loadWorldFromFile = undefined

unsafeSaveWorldToFile :: World -> World
unsafeSaveWorldToFile w = unsafePerformIO $ do
  timeAsString <- show . floor <$> getPOSIXTime
  currentDir <- getCurrentDirectory
  let filename = currentDir ++ "/maze_game_save_" ++ timeAsString
  encodeFile filename w
  return w
