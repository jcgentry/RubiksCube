module Cube(
  Cube(..), Color(..), Face(..), Turn, startingCube, solved, frontTurn, rightTurn, upTurn, leftTurn, backTurn, downTurn,
  frontTurn', rightTurn', upTurn', leftTurn', backTurn', downTurn', frontTurn2, rightTurn2, upTurn2, leftTurn2,
  backTurn2, downTurn2, turns) where

import qualified System.Console.ANSI as ANSI
import Face
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
              p1 = p1 (front cube), p2 = p2 (front cube), p3 = p3 (front cube)


  },
  front = (front cube)  {
              p1 = p1 (right cube), p2 = p2 (right cube), p3 = p3 (right cube)


  },
  right = (right cube) {
              p1 = p1 (back cube), p2 = p2 (back cube), p3 = p3 (back cube)


  },
  back  = (back cube) {
              p1 = p1 (left cube), p2 = p2 (left cube), p3 = p3 (left cube)


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

backTurn :: Turn
backTurn cube = cube {
  up    = (up cube) {
            p1 = p3 (right cube), p2 = p6 (right cube), p3 = p9 (right cube)


  },
  left  = (left cube) {
            p1 = p3 (up cube),
            p4 = p2 (up cube),
            p7 = p1 (up cube)
  },
  front = front cube,
  right = (right cube) {
                                                        p3 = p9 (down cube),
                                                        p6 = p8 (down cube),
                                                        p9 = p7 (down cube)
  },
  back  = (back cube) {
            p1 = p7 (back cube), p2 = p4 (back cube), p3 = p1 (back cube),
            p4 = p8 (back cube), p5 = p5 (back cube), p6 = p2 (back cube),
            p7 = p9 (back cube), p8 = p6 (back cube), p9 = p3 (back cube)
   },
  down  = (down cube) {


            p7 = p1 (left cube),  p8 = p4 (left cube),  p9 = p7 (left cube)
  }
}

downTurn :: Turn
downTurn cube = cube {
  up    = up cube,
  left  = (left cube) {


            p7 = p7 (back cube),  p8 = p8 (back cube),  p9 = p9 (back cube)
  },
  front = (front cube) {


            p7 = p7 (left cube),  p8 = p8 (left cube),  p9 = p9 (left cube)
  },
  right = (right cube) {


            p7 = p7 (front cube),  p8 = p8 (front cube),  p9 = p9 (front cube)
  },
  back  = (back cube) {


            p7 = p7 (right cube),  p8 = p8 (right cube),  p9 = p9 (right cube)
   },
  down  = (down cube) {
            p1 = p7 (down cube), p2 = p4 (down cube), p3 = p1 (down cube),
            p4 = p8 (down cube), p5 = p5 (down cube), p6 = p2 (down cube),
            p7 = p9 (down cube), p8 = p6 (down cube), p9 = p3 (down cube)
  }
}

-- Okay, I've gotten kind of lazy for these...
frontTurn'  = frontTurn . frontTurn . frontTurn
rightTurn'  = rightTurn . rightTurn . rightTurn
upTurn'     = upTurn . upTurn . upTurn
leftTurn'   = leftTurn . leftTurn . leftTurn
backTurn'   = backTurn . backTurn . backTurn
downTurn'   = downTurn . downTurn . downTurn

frontTurn2  = frontTurn . frontTurn
rightTurn2  = rightTurn . rightTurn
upTurn2     = upTurn . upTurn
leftTurn2   = leftTurn . leftTurn
backTurn2   = backTurn . backTurn
downTurn2   = downTurn . downTurn

turns :: [Turn]
turns = [frontTurn, rightTurn, upTurn, leftTurn, backTurn, downTurn, frontTurn', rightTurn', upTurn', leftTurn',
          backTurn', downTurn', frontTurn2, rightTurn2, upTurn2, leftTurn2, backTurn2, downTurn2]