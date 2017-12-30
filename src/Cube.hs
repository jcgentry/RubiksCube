module Cube(
  Cube(..), Color(..), Face(..), startingCube, solved) where

import Face
import Color
import Pretty

data Cube = Cube {
                  up    :: Face,
  left  :: Face,  front :: Face,  right :: Face,  back  :: Face,
                  down  :: Face
} deriving (Show, Eq)

instance Printable Cube where
  pretty (Cube u l f r b d) =
      newLine ++
      emptyRow ++
      printRow (row u 0) ++
      emptyRow ++
      emptyRow ++
      newLine ++

      emptyRow ++
      printRow(row u 1) ++
      emptyRow ++
      emptyRow ++
      newLine ++

      emptyRow ++
      printRow(row u 2) ++
      emptyRow ++
      emptyRow ++
      newLine ++

      printRow(row l 0) ++
      printRow(row f 0) ++
      printRow(row r 0) ++
      printRow(row b 0) ++
      newLine ++

      printRow(row l 1) ++
      printRow(row f 1) ++
      printRow(row r 1) ++
      printRow(row b 1) ++
      newLine ++

      printRow(row l 2) ++
      printRow(row f 2) ++
      printRow(row r 2) ++
      printRow(row b 2) ++
      newLine ++

      emptyRow ++
      printRow(row d 0) ++
      emptyRow ++
      emptyRow ++
      newLine ++

      emptyRow ++
      printRow(row d 1) ++
      emptyRow ++
      emptyRow ++
      newLine ++

      emptyRow ++
      printRow (row d 2) ++
      emptyRow ++
      emptyRow


emptyRow :: [PrettyElement]
emptyRow = [Spaces 6]

printRow :: [Color] -> [PrettyElement]
printRow = map (`ColorText` "* ")


newLine :: [PrettyElement]
newLine = [NewLine]

{--
  "Western" color scheme as described here: https://ruwix.com/the-rubiks-cube/japanese-western-color-schemes/.
 -}
startingCube = Cube {
                                    up    = faceWithOneColor White,
  left  = faceWithOneColor Orange,  front = faceWithOneColor Green, right = faceWithOneColor Red, back  = faceWithOneColor Blue,
                                    down  = faceWithOneColor Yellow
}

solved :: Cube -> Bool
solved (Cube u l f r b d) = faceAllSameColor f &&
                faceAllSameColor l &&
                faceAllSameColor u &&
                faceAllSameColor d &&
                faceAllSameColor r &&
                faceAllSameColor b

