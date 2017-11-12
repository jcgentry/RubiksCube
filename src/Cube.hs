module Cube(Cube(..), Color(..), Face(..), startingCube, solved) where

import qualified System.Console.ANSI as ANSI
import Prelude hiding (print)

square = "*"

class (Show a) => Print a where
  print :: a -> IO ()
  println :: a -> IO ()
  print = putStr . show
  println x = do
                print x
                putStrLn ""

data Color = Red | Orange | Yellow | Green | Blue | White deriving (Eq, Show)

instance Print Color where
  print Red    = do
                   ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
                   putStr square
                   putStr " "
                   ANSI.setSGR [ANSI.Reset]
  print Orange = do
                   ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Magenta]
                   putStr square
                   putStr " "
                   ANSI.setSGR [ANSI.Reset]
  print Yellow = do
                   ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
                   putStr square
                   putStr " "
                   ANSI.setSGR [ANSI.Reset]
  print Green  = do
                   ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
                   putStr square
                   putStr " "
                   ANSI.setSGR [ANSI.Reset]
  print Blue   = do
                   ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
                   putStr square
                   putStr " "
                   ANSI.setSGR [ANSI.Reset]
  print White  = do
                   ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
                   putStr square
                   putStr " "
                   ANSI.setSGR [ANSI.Reset]

instance Print x => Print [x] where
  print (c:cs) = do
                  print c
                  print cs
  print [] = return ()

data Face = Face {
  xlu :: Color,
  xu  :: Color,
  xru :: Color,
  xl  :: Color,
  x   :: Color,
  xr  :: Color,
  xrd :: Color,
  xd  :: Color,
  xld :: Color
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

data Cube = Cube {
  f :: Face,
  l :: Face,
  u :: Face,
  d :: Face,
  r :: Face,
  b :: Face
} deriving (Show, Eq)

emptyRow :: IO ()
emptyRow = putStr "      "

instance Print Cube where
  print (Cube f l u d r b) = do
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
  f = faceWithOneColor Orange,
  l = faceWithOneColor Green,
  u = faceWithOneColor Yellow,
  d = faceWithOneColor White,
  r = faceWithOneColor Blue,
  b = faceWithOneColor Red
}

solved :: Cube -> Bool
solved (Cube f l u d r b) = faceAllSameColor f &&
                faceAllSameColor l &&
                faceAllSameColor u &&
                faceAllSameColor d &&
                faceAllSameColor r &&
                faceAllSameColor b

--printCube :: Cube -> IO ()
--printCube =