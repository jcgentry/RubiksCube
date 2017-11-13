module Face where

import Color
import Prelude hiding (print)
import Print

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

faceWithOneColor :: Color -> Face
faceWithOneColor color = Face color color color color color color color color color

faceAllSameColor :: Face -> Bool
faceAllSameColor (Face c1 c2 c3 c4 c5 c6 c7 c8 c9) = all (c1 ==) [c2, c3, c4, c5, c6, c7, c8, c9]