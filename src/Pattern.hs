module Pattern (Pattern (Pattern), checkerboard, combine, randomPattern) where

import Cube
import Print
import System.Random
import Debug.Trace
import Turn
import Applicable
import Control.Monad.Random


godsNumber = 20

newtype Pattern = Pattern [Turn]

instance Applicable Pattern where
  apply (Pattern turns) cube = foldl (\cube turn -> apply turn cube) cube turns

showPattern :: Cube -> Pattern -> IO ()
showPattern cube pattern = dump $ apply pattern cube

showPatternIO :: Cube -> IO Pattern -> IO ()
showPatternIO cube patternIO = do
                                pattern <- patternIO
                                showPattern cube pattern

combine :: Pattern -> Pattern -> Pattern
combine (Pattern t1s) (Pattern t2s) = Pattern (t1s ++ t2s)


checkerboard :: Pattern
checkerboard = Pattern [l2, r2, u2, d2, f2, b2]

randomPattern :: (RandomGen g) => Rand g Pattern
randomPattern = fmap Pattern $ sequence (replicate godsNumber randomTurn)