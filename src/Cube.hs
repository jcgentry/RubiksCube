module Cube(
  Cube(..), Color(..), Face(..), startingCube, solved) where

import qualified System.Console.ANSI as ANSI
import Face
import Color
import Dump

data Cube = Cube {
                  up    :: Face,
  left  :: Face,  front :: Face,  right :: Face,  back  :: Face,
                  down  :: Face
} deriving (Show, Eq)

instance Dump Cube where
  dump (Cube u l f r b d) = do
    emptyRow
    dumpln $ row u 0

    emptyRow
    dumpln $ row u 1

    emptyRow
    dumpln $ row u 2

    dump $ row l 0
    dump $ row f 0
    dump $ row r 0
    dumpln $ row b 0

    dump $ row l 1
    dump $ row f 1
    dump $ row r 1
    dumpln $ row b 1

    dump $ row l 2
    dump $ row f 2
    dump $ row r 2
    dumpln $ row b 2

    emptyRow
    dumpln $ row d 0

    emptyRow
    dumpln $ row d 1

    emptyRow
    dumpln $ row d 2

emptyRow :: IO ()
emptyRow = putStr "      "

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

