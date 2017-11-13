module Cube(Cube(..), Color(..), Face(..), startingCube, solved, frontTurn) where

import qualified System.Console.ANSI as ANSI
import Prelude hiding (print)

square = "*"

type Turn = Cube -> Cube

class (Show a) => Print a where
  print :: a -> IO ()
  println :: a -> IO ()
  print = putStr . show
  println x = do
                print x
                putStrLn ""

data Color = Red | Orange | Yellow | Green | Blue | White deriving (Eq, Show)

ansiColor :: Color -> ANSI.Color
ansiColor Red = ANSI.Red
ansiColor Orange = ANSI.Magenta
ansiColor Yellow = ANSI.Yellow
ansiColor Green = ANSI.Green
ansiColor Blue = ANSI.Blue
ansiColor White = ANSI.Black

instance Print Color where
  print color = do
                   ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (ansiColor color)]
                   putStr square
                   putStr " "
                   ANSI.setSGR [ANSI.Reset]

instance Print x => Print [x] where
  print (c:cs) = do
                  print c
                  print cs
  print [] = return ()

data Face = Face {
  xlu :: Color, xu  :: Color, xru :: Color,
  xl  :: Color, x   :: Color, xr  :: Color,
  xld :: Color, xd  :: Color, xrd :: Color
} deriving (Show, Eq)

instance Print Face where
  print f = do
              print (xlu f)
              print (xu f)
              println (xru f)
              print (xl f)
              print (x f)
              println (xr f)
              print (xrd f)
              print (xd f)
              println (xld f)

row :: Face -> Int -> [Color]
row face 0 = [xlu face, xu face, xru face]
row face 1 = [xl face, x face, xr face]
row face 2 = [xld face, xd face, xrd face]

type Side = Int


data Cube = Cube {
                up :: Face,
  left :: Face, front :: Face,  right :: Face,  back :: Face,
                down :: Face
} deriving (Show, Eq)

emptyRow :: IO ()
emptyRow = putStr "      "

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

faceWithOneColor :: Color -> Face
faceWithOneColor color = Face color color color color color color color color color

faceAllSameColor :: Face -> Bool
faceAllSameColor (Face c1 c2 c3 c4 c5 c6 c7 c8 c9) = all (c1 ==) [c2, c3, c4, c5, c6, c7, c8, c9]

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

--clockwiseTurn :: Cube -> Face -> Cube


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