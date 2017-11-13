module Cube(Cube(..), Color(..), Face(..), startingCube, solved, frontTurn) where

import qualified System.Console.ANSI as ANSI
import Face
import Prelude hiding (print)
import Color
import Print

type Turn = Cube -> Cube

data Cube = Cube {
                up :: Face,
  left :: Face, front :: Face,  right :: Face,  back :: Face,
                down :: Face
} deriving (Show, Eq)

instance Print Cube where
  print (Cube u l f r b d) = do
    emptyRow
    println $ row u 0

    emptyRow
    println $ row u 1

    emptyRow
    println $ row u 2

    print $ row l 0
    print $ row f 0
    print $ row r 0
    println $ row b 0

    print $ row l 1
    print $ row f 1
    print $ row r 1
    println $ row b 1

    print $ row l 2
    print $ row f 2
    print $ row r 2
    println $ row b 2

    emptyRow
    println $ row d 0

    emptyRow
    println $ row d 1

    emptyRow
    println $ row d 2

emptyRow :: IO ()
emptyRow = putStr "      "

startingCube = Cube {
                                  up = faceWithOneColor Yellow,
  left = faceWithOneColor Green,  front = faceWithOneColor Orange,  right = faceWithOneColor Blue, back = faceWithOneColor Red,
                                  down = faceWithOneColor White
}

solved :: Cube -> Bool
solved (Cube u l f r b d) = faceAllSameColor f &&
                faceAllSameColor l &&
                faceAllSameColor u &&
                faceAllSameColor d &&
                faceAllSameColor r &&
                faceAllSameColor b

frontTurn :: Turn
frontTurn cube = cube {
  up    = (up cube)     {

                          xld = xrd (left cube), xd   = xr (left cube),   xrd = xlu (left cube)
          },
  left  = (left cube)   {                                                 xru = xlu (down cube),
                                                                          xr  = xu  (down cube),
                                                                          xrd = xru (down cube)
          },
  front = (front cube)  { xlu = xld (front cube), xu  = xl (front cube),  xru = xlu (front cube),
                          xl  = xd  (front cube), x   = x (front cube),   xr  = xu (front cube),
                          xld = xrd (front cube), xd  = xr (front cube),  xrd = xru (front cube)
          },
  right = (right cube)  { xlu = xld (up cube),
                          xl  = xd  (up cube),
                          xld = xrd (up cube)
          },
  back = back cube,
  down  = (down cube)   { xlu = xlu (right cube), xu  = xr (right cube),  xru = xru (right cube)

          }
}