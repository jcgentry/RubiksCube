module Turn (Turn(..), applyTurn) where

import Cube
import Control.Monad.Random
import Pretty

type Rotation = (Cube -> Cube)

frontRotation cube =
  cube
  { up = (up cube) {p7 = p9 (left cube), p8 = p6 (left cube), p9 = p3 (left cube)}
  , left = (left cube) {p3 = p1 (down cube), p6 = p2 (down cube), p9 = p3 (down cube)}
  , front =
      (front cube)
      { p1 = p7 (front cube)
      , p2 = p4 (front cube)
      , p3 = p1 (front cube)
      , p4 = p8 (front cube)
      , p5 = p5 (front cube)
      , p6 = p2 (front cube)
      , p7 = p9 (front cube)
      , p8 = p6 (front cube)
      , p9 = p3 (front cube)
      }
  , right = (right cube) {p1 = p7 (up cube), p4 = p8 (up cube), p7 = p9 (up cube)}
  , back = back cube
  , down = (down cube) {p1 = p7 (right cube), p2 = p4 (right cube), p3 = p1 (right cube)}
  }

rightRotation cube =
  cube
  { up = (up cube) {p3 = p3 (front cube), p6 = p6 (front cube), p9 = p9 (front cube)}
  , left = left cube
  , front = (front cube) {p3 = p3 (down cube), p6 = p6 (down cube), p9 = p9 (down cube)}
  , right =
      (right cube)
      { p1 = p7 (right cube)
      , p2 = p4 (right cube)
      , p3 = p1 (right cube)
      , p4 = p8 (right cube)
      , p5 = p5 (right cube)
      , p6 = p2 (right cube)
      , p7 = p9 (right cube)
      , p8 = p6 (right cube)
      , p9 = p3 (right cube)
      }
  , back = (back cube) {p1 = p9 (up cube), p4 = p6 (up cube), p7 = p3 (up cube)}
  , down = (down cube) {p3 = p7 (back cube), p6 = p4 (back cube), p9 = p1 (back cube)}
  }

upRotation cube =
  cube
  { up =
      (up cube)
      { p1 = p7 (up cube)
      , p2 = p4 (up cube)
      , p3 = p1 (up cube)
      , p4 = p8 (up cube)
      , p5 = p5 (up cube)
      , p6 = p2 (up cube)
      , p7 = p9 (up cube)
      , p8 = p6 (up cube)
      , p9 = p3 (up cube)
      }
  , left = (left cube) {p1 = p1 (front cube), p2 = p2 (front cube), p3 = p3 (front cube)}
  , front = (front cube) {p1 = p1 (right cube), p2 = p2 (right cube), p3 = p3 (right cube)}
  , right = (right cube) {p1 = p1 (back cube), p2 = p2 (back cube), p3 = p3 (back cube)}
  , back = (back cube) {p1 = p1 (left cube), p2 = p2 (left cube), p3 = p3 (left cube)}
  , down = down cube
  }

leftRotation cube =
  cube
  { up = (up cube) {p1 = p9 (back cube), p4 = p6 (back cube), p7 = p3 (back cube)}
  , left =
      (left cube)
      { p1 = p7 (left cube)
      , p2 = p4 (left cube)
      , p3 = p1 (left cube)
      , p4 = p8 (left cube)
      , p5 = p5 (left cube)
      , p6 = p2 (left cube)
      , p7 = p9 (left cube)
      , p8 = p6 (left cube)
      , p9 = p3 (left cube)
      }
  , front = (front cube) {p1 = p1 (up cube), p4 = p4 (up cube), p7 = p7 (up cube)}
  , right = right cube
  , back = (back cube) {p3 = p7 (down cube), p6 = p4 (down cube), p9 = p1 (down cube)}
  , down = (down cube) {p1 = p1 (front cube), p4 = p4 (front cube), p7 = p7 (front cube)}
  }

backRotation cube
  = cube{up =
           (up cube){p1 = p3 (right cube), p2 = p6 (right cube),
                     p3 = p9 (right cube)},
         left =
           (left cube){p1 = p3 (up cube), p4 = p2 (up cube),
                       p7 = p1 (up cube)},
         front = front cube,
         right =
           (right cube){p3 = p9 (down cube), p6 = p8 (down cube),
                        p9 = p7 (down cube)},
         back =
           (back cube){p1 = p7 (back cube), p2 = p4 (back cube),
                       p3 = p1 (back cube), p4 = p8 (back cube), p5 = p5 (back cube),
                       p6 = p2 (back cube), p7 = p9 (back cube), p8 = p6 (back cube),
                       p9 = p3 (back cube)},
         down =
           (down cube){p7 = p1 (left cube), p8 = p4 (left cube),
                       p9 = p7 (left cube)}}

downRotation cube =
  cube
  { up = up cube
  , left = (left cube) {p7 = p7 (back cube), p8 = p8 (back cube), p9 = p9 (back cube)}
  , front = (front cube) {p7 = p7 (left cube), p8 = p8 (left cube), p9 = p9 (left cube)}
  , right = (right cube) {p7 = p7 (front cube), p8 = p8 (front cube), p9 = p9 (front cube)}
  , back = (back cube) {p7 = p7 (right cube), p8 = p8 (right cube), p9 = p9 (right cube)}
  , down =
      (down cube)
      { p1 = p7 (down cube)
      , p2 = p4 (down cube)
      , p3 = p1 (down cube)
      , p4 = p8 (down cube)
      , p5 = p5 (down cube)
      , p6 = p2 (down cube)
      , p7 = p9 (down cube)
      , p8 = p6 (down cube)
      , p9 = p3 (down cube)
      }
  }

data Turn = F | R | U | L | B | D | F' | R' | U' | L' | B' | D' | F2 | R2 | U2 | L2 | B2 | D2 deriving (Enum, Bounded)

instance Show Turn where
  show F = "f"
  show R = "r"
  show U = "u"
  show L = "l"
  show B = "b"
  show D = "d"
  show F' = "f'"
  show R' = "r'"
  show U' = "u'"
  show L' = "l'"
  show B' = "b'"
  show D' = "d'"
  show F2 = "f2"
  show R2 = "r2"
  show U2 = "u2"
  show L2 = "l2"
  show B2 = "b2"
  show D2 = "d2"

rotations :: Turn -> [Rotation]
rotations F = [frontRotation]
rotations R = [rightRotation]
rotations U = [upRotation]
rotations L = [leftRotation]
rotations B = [backRotation]
rotations D = [downRotation]
rotations F' = [frontRotation, frontRotation, frontRotation]
rotations R' = [rightRotation, rightRotation, rightRotation]
rotations U' = [upRotation, upRotation, upRotation]
rotations L' = [leftRotation, leftRotation, leftRotation]
rotations B' = [backRotation, backRotation, backRotation]
rotations D' = [downRotation, downRotation, downRotation]
rotations F2 = [frontRotation, frontRotation]
rotations R2 = [rightRotation, rightRotation]
rotations U2 = [upRotation, upRotation]
rotations L2 = [leftRotation, leftRotation]
rotations B2 = [backRotation, backRotation]
rotations D2 = [downRotation, downRotation]


applyTurn :: Turn -> Cube -> Cube
applyTurn t cube = foldl (\c r -> r c) cube (rotations t)

instance Printable Turn where
  pretty t = [Text (show t)]

instance Random Turn where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)
