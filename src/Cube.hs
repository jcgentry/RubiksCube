module Cube(Cube(..), Color(..), Face(..), startingCube, solved, frontTurn, rightTurn, upTurn, leftTurn) where

import qualified System.Console.ANSI as ANSI
import Face
import Prelude hiding (print)
import Color
import Print

{--
  This is a simplistic representation for now, where each turn type is written out explicitly.
-}

type Turn = Cube -> Cube

data Cube = Cube {
                  up    :: Face,
  left  :: Face,  front :: Face,  right :: Face,  back  :: Face,
                  down  :: Face
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

frontTurn :: Turn
frontTurn cube = cube {
  up    = (up cube) {


            p7 = p9 (left cube),  p8 = p6 (left cube),  p9 = p3 (left cube)
          },
  left  = (left cube) {
                                                        p3 = p1 (down cube),
                                                        p6 = p2 (down cube),
                                                        p9 = p3 (down cube)
          },
  front = (front cube)  {
            p1 = p7 (front cube), p2 = p4 (front cube), p3 = p1 (front cube),
            p4 = p8 (front cube), p5 = p5 (front cube), p6 = p2 (front cube),
            p7 = p9 (front cube), p8 = p6 (front cube), p9 = p3 (front cube)
          },
  right = (right cube) {
            p1 = p7 (up cube),
            p4 = p8 (up cube),
            p7 = p9 (up cube)
          },
  back = back cube,
  down  = (down cube) {
            p1 = p7 (right cube), p2 = p4 (right cube), p3 = p1 (right cube)


          }
}

rightTurn :: Turn
rightTurn cube = cube {
  up    = (up cube) {
                                                        p3 = p3 (front cube),
                                                        p6 = p6 (front cube),
                                                        p9 = p9 (front cube)
          },
  left  = left cube,
  front = (front cube)  {
                                                        p3 = p3 (down cube),
                                                        p6 = p6 (down cube),
                                                        p9 = p9 (down cube)
          },
  right = (right cube) {
            p1 = p7 (right cube), p2 = p4 (right cube), p3 = p1 (right cube),
            p4 = p8 (right cube), p5 = p5 (right cube), p6 = p2 (right cube),
            p7 = p9 (right cube), p8 = p6 (right cube), p9 = p3 (right cube)
          },
  back  = (back cube) {
            p1 = p9 (up cube),
            p4 = p6 (up cube),
            p7 = p3 (up cube)
          },
  down  = (down cube) {
                                                        p3 = p7 (back cube),
                                                        p6 = p4 (back cube),
                                                        p9 = p1 (back cube)
          }
}

upTurn :: Turn
upTurn cube = cube {
  up    = (up cube) {
              p1 = p7 (up cube), p2 = p4 (up cube), p3 = p1 (up cube),
              p4 = p8 (up cube), p5 = p5 (up cube), p6 = p2 (up cube),
              p7 = p9 (up cube), p8 = p6 (up cube), p9 = p3 (up cube)
  },
  left  = (left cube) {
              p1 = p1 (front cube), p2 = p2 (front cube), p3 = p2 (front cube)


  },
  front = (front cube)  {
              p1 = p1 (right cube), p2 = p2 (right cube), p3 = p2 (right cube)


  },
  right = (right cube) {
              p1 = p1 (back cube), p2 = p2 (back cube), p3 = p2 (back cube)


  },
  back  = (back cube) {
              p1 = p1 (left cube), p2 = p2 (left cube), p3 = p2 (left cube)


  },
  down  = down cube
}

leftTurn :: Turn
leftTurn cube = cube {
  up    = (up cube) {
            p1 = p9 (back cube),
            p4 = p6 (back cube),
            p7 = p3 (back cube)
  },
  left  = (left cube) {
            p1 = p7 (left cube), p2 = p4 (left cube), p3 = p1 (left cube),
            p4 = p8 (left cube), p5 = p5 (left cube), p6 = p2 (left cube),
            p7 = p9 (left cube), p8 = p6 (left cube), p9 = p3 (left cube)
  },
  front = (front cube)  {
            p1 = p1 (up cube),
            p4 = p4 (up cube),
            p7 = p7 (up cube)
  },
  right = right cube,
  back  = (back cube) {
                                                      p3 = p7 (down cube),
                                                      p6 = p4 (down cube),
                                                      p9 = p1 (down cube)
  },
  down  = (down cube) {
            p1 = p1 (front cube),
            p4 = p4 (front cube),
            p7 = p7 (front cube)
  }
}