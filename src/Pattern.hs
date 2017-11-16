module Pattern where

import Cube
import Print

newtype Pattern = Pattern [Turn]

apply :: Pattern -> Cube -> Cube
apply (Pattern turns) cube = foldl (\cube turn -> turn cube) cube turns

showPattern :: Cube -> Pattern -> IO()
showPattern cube pattern = dump $ apply pattern cube

checkerboard :: Pattern
checkerboard = Pattern [leftTurn2, rightTurn2, upTurn2, downTurn2, frontTurn2, backTurn2]

