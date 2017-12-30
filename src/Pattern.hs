module Pattern (Pattern (Pattern), checkerboard, combine, applyPattern, randomPattern) where

import Cube
import Pretty
import System.Random
import Debug.Trace
import Turn
import Control.Monad.Random
import Control.Monad
import Data.List


godsNumber = 20

newtype Pattern = Pattern [Turn]

applyPattern (Pattern turns) cube = foldl (flip applyTurn) cube turns

combine :: Pattern -> Pattern -> Pattern
combine (Pattern t1s) (Pattern t2s) = Pattern (t1s ++ t2s)

instance Printable Pattern where
  pretty (Pattern turns) = Text " " `intersperse` (turns >>= pretty)


checkerboard :: Pattern
checkerboard = Pattern [L2, R2, U2, D2, F2, B2]

randomPattern :: IO Pattern
randomPattern = do
                  g <- newStdGen
                  let ts = randoms g :: [Turn]
                  return (Pattern (take godsNumber ts))