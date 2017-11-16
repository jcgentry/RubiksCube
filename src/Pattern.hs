module Pattern where

import Cube
import Print

newtype Pattern = Pattern [Turn]

apply :: Cube -> Pattern -> Cube
apply cube (Pattern turns) = foldl (\cube turn -> turn cube) cube turns

showPattern :: Cube -> Pattern -> IO()
showPattern cube pattern = dump $ apply cube pattern

checkerboard :: Pattern
checkerboard = Pattern [leftTurn2, rightTurn2, upTurn2, downTurn2, frontTurn2, backTurn2]

