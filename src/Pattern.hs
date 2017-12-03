module Pattern (Pattern (Pattern), checkerboard, combine, applyPattern, randomPattern) where

import Cube
import Dump
import System.Random
import Debug.Trace
import Turn
import Control.Monad.Random


godsNumber = 20

newtype Pattern = Pattern [Turn]

applyPattern (Pattern turns) cube = foldl (\cube turn -> applyTurn turn cube) cube turns

showPattern :: Cube -> Pattern -> IO ()
showPattern cube pattern = dump $ applyPattern pattern cube

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