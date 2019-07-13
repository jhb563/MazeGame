module Main where

import qualified Player as P
import qualified Runner as R

main :: IO ()
main = R.main P.evaluateWorld
