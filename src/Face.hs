module Face where

import Color
import Prelude hiding (print)
import Print

data Face = Face {
  p1 :: Color, p2 :: Color, p3 :: Color,
  p4 :: Color, p5 :: Color, p6 :: Color,
  p7 :: Color, p8 :: Color, p9 :: Color
} deriving (Show, Eq)

instance Print Face where
  print f = do
              print (p1 f)
              print (p2 f)
              println (p3 f)
              print (p4 f)
              print (p5 f)
              println (p6 f)
              print (p9 f)
              print (p8 f)
              println (p7 f)

row :: Face -> Int -> [Color]
row face 0 = [p1 face, p2 face, p3 face]
row face 1 = [p4 face, p5 face, p6 face]
row face 2 = [p7 face, p8 face, p9 face]

faceWithOneColor :: Color -> Face
faceWithOneColor color = Face color color color color color color color color color

faceAllSameColor :: Face -> Bool
faceAllSameColor (Face c1 c2 c3 c4 c5 c6 c7 c8 c9) = all (c1 ==) [c2, c3, c4, c5, c6, c7, c8, c9]