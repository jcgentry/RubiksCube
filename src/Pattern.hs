module Pattern (Pattern (Pattern), checkerboard, combine, applyPattern, randomPattern, cubeWithRandomPattern) where

import Cube
import Pretty
import System.Random
import Debug.Trace
import Turn
import Control.Monad.Random
import Control.Monad
import Data.List


godsNumber = 20

newtype Pattern = Pattern [Turn] deriving Show

applyPattern :: Pattern -> Cube -> Cube
applyPattern (Pattern turns) cube = foldl (flip applyTurn) cube turns

combine :: Pattern -> Pattern -> Pattern
combine (Pattern t1s) (Pattern t2s) = Pattern (t1s ++ t2s)

checkerboard :: Pattern
checkerboard = Pattern [L2, R2, U2, D2, F2, B2]

randomPattern :: IO Pattern
randomPattern = do
                  g <- newStdGen
                  let ts = randoms g :: [Turn]
                  return (Pattern (take godsNumber ts))

cubeWithRandomPattern :: IO Cube
cubeWithRandomPattern = do
  p <- randomPattern
  let cube = applyPattern p startingCube
  print p
  prettyPrint cube
  return cube
