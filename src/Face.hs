module Face where

import Color
import Prelude
import Print

{--
  There *is* more-standard notation for the positions on the face, but I find `p1`, `p2`, `p3`, _etc._
  easier to deal with
--}
data Face = Face {
  p1 :: Color, p2 :: Color, p3 :: Color,
  p4 :: Color, p5 :: Color, p6 :: Color,
  p7 :: Color, p8 :: Color, p9 :: Color
} deriving (Show, Eq)

instance Print Face where
  dump f = do
              dump (p1 f)
              dump (p2 f)
              dumpln (p3 f)
              dump (p4 f)
              dump (p5 f)
              dumpln (p6 f)
              dump (p9 f)
              dump (p8 f)
              dumpln (p7 f)

row :: Face -> Int -> [Color]
row face 0 = [p1 face, p2 face, p3 face]
row face 1 = [p4 face, p5 face, p6 face]
row face 2 = [p7 face, p8 face, p9 face]

faceWithOneColor :: Color -> Face
faceWithOneColor color = Face color color color color color color color color color

faceAllSameColor :: Face -> Bool
faceAllSameColor (Face c1 c2 c3 c4 c5 c6 c7 c8 c9) = all (c1 ==) [c2, c3, c4, c5, c6, c7, c8, c9]