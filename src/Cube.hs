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
      emptyRow ++
      map ChangeColor (row u 0) ++
      newLine ++

      emptyRow ++
      map ChangeColor (row u 1) ++
      newLine ++

      emptyRow ++
      map ChangeColor (row u 2) ++
      newLine ++

      map ChangeColor (row l 0) ++
      map ChangeColor (row f 0) ++
      map ChangeColor (row r 0) ++
      map ChangeColor (row b 0) ++
      newLine ++

      map ChangeColor (row l 1) ++
      map ChangeColor (row f 1) ++
      map ChangeColor (row r 1) ++
      map ChangeColor (row b 1) ++
      newLine ++

      map ChangeColor (row l 2) ++
      map ChangeColor (row f 2) ++
      map ChangeColor (row r 2) ++
      map ChangeColor (row b 2) ++
      newLine ++

      emptyRow ++
      map ChangeColor (row d 0) ++
      newLine ++

      emptyRow ++
      map ChangeColor (row d 1) ++
      newLine ++

      emptyRow ++
      map ChangeColor (row d 2)


emptyRow :: [PrettyElement]
emptyRow = [Text "      "]

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

