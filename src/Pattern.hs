module Pattern where

import Cube

newtype Pattern = Pattern [Turn]

apply :: Cube -> Pattern -> Cube
apply cube (Pattern turns) = foldl (\cube turn -> turn cube) cube turns

checkerboard :: Pattern
checkerboard = Pattern [leftTurn2, rightTurn2, upTurn2, downTurn2, frontTurn2, backTurn2]

