module Pattern where

import Cube
import Print
import System.Random
import Debug.Trace
import Turn

godsNumber = 20

newtype Pattern = Pattern [Turn]

instance Applicable Pattern where
  (Pattern ts1) +++ (Pattern ts2) = Pattern (ts1 ++ ts2)
  apply (Pattern turns) cube = foldl (\cube (Turn turn) -> turn cube) cube turns

showPattern :: Cube -> Pattern -> IO ()
showPattern cube pattern = dump $ apply pattern cube

showPatternIO :: Cube -> IO Pattern -> IO ()
showPatternIO cube patternIO = do
                                pattern <- patternIO
                                showPattern cube pattern

checkerboard :: Pattern
checkerboard = Pattern [l2, r2, u2, d2, f2, b2]

randomPattern :: IO Pattern
randomPattern = do
                  count <- getStdRandom (randomR (1, godsNumber))
                  gen <- getStdGen
                  return $ Pattern (map (turns !!) (take count (randomRs (0, ((length turns) - 1)) gen)))
