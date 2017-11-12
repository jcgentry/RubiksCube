module Cube(Cube(..), Color(..), Face(..), startingCube) where

data Color = Red | Orange | Yellow | Green | Blue | White deriving (Show, Eq)

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

data Cube = Cube {
  f :: Face,
  l :: Face,
  u :: Face,
  d :: Face,
  r :: Face,
  b :: Face
} deriving (Show, Eq)

faceWithOneColor :: Color -> Face
faceWithOneColor color = Face color color color color color color color color color

startingCube = Cube {
  f = faceWithOneColor Orange,
  l = faceWithOneColor Green,
  u = faceWithOneColor Yellow,
  d = faceWithOneColor White,
  r = faceWithOneColor Blue,
  b = faceWithOneColor Red
}

