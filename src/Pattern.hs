module Pattern where

import Cube
import Print
import System.Random
import Debug.Trace

godsNumber = 20

newtype Pattern = Pattern [Turn]

apply :: Pattern -> Cube -> Cube
apply (Pattern turns) cube = foldl (\cube turn -> turn cube) cube turns

showPattern :: Cube -> Pattern -> IO ()
showPattern cube pattern = dump $ apply pattern cube

showPatternIO :: Cube -> IO Pattern -> IO ()
showPatternIO cube patternIO = do
                                pattern <- patternIO
                                showPattern cube pattern

checkerboard :: Pattern
checkerboard = Pattern [leftTurn2, rightTurn2, upTurn2, downTurn2, frontTurn2, backTurn2]

randomPattern :: IO Pattern
randomPattern = do
                  count <- getStdRandom (randomR (1, godsNumber))
                  gen <- getStdGen
                  return $ Pattern (map (turns !!) (take count (randomRs (0, ((length turns) - 1)) gen)))
