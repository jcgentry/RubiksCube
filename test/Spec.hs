import Test.Hspec
import Cube
import Pattern

main :: IO ()
main = hspec $ do
  describe "Cube" $ do
    it "new cube" $
      startingCube `shouldBe`
        Cube {
          front =
            Face
              Green   Green   Green
              Green   Green   Green
              Green   Green   Green,
          left =
            Face
              Orange  Orange  Orange
              Orange  Orange  Orange
              Orange  Orange  Orange,
          up =
            Face
              White   White   White
              White   White   White
              White   White   White,
          down =
            Face
              Yellow  Yellow  Yellow
              Yellow  Yellow  Yellow
              Yellow  Yellow  Yellow,
          right =
            Face
              Red     Red     Red
              Red     Red     Red
              Red     Red     Red,
          back =
            Face
              Blue    Blue    Blue
              Blue    Blue    Blue
              Blue    Blue    Blue
        }

    it "new is solved" $
      solved startingCube `shouldBe` True

  describe "turns" $ do
    it "front turn" $
      frontTurn startingCube `shouldBe`
        Cube {
          front = Face
                    Green Green Green
                    Green Green Green
                    Green Green Green,
          left = Face
                    Orange Orange Yellow
                    Orange Orange Yellow
                    Orange Orange Yellow,
          up = Face
                    White White White
                    White White White
                    Orange  Orange  Orange,
          down = Face
                    Red    Red    Red
                    Yellow Yellow Yellow
                    Yellow Yellow Yellow,
          right = Face
                    White Red Red
                    White Red Red
                    White Red Red,
          back = Face
                    Blue Blue Blue
                    Blue Blue Blue
                    Blue Blue Blue
        }

    it "not solved if turned" $
      solved (frontTurn startingCube) `shouldBe` False

    it "4 front turns solves it" $
      solved ((frontTurn . frontTurn . frontTurn . frontTurn) startingCube) `shouldBe` True

    it "two front turns" $
      (frontTurn . frontTurn) startingCube `shouldBe`
        Cube {
          front = Face
                    Green Green Green
                    Green Green Green
                    Green Green Green,
          left = Face
                    Orange Orange Red
                    Orange Orange Red
                    Orange Orange Red,
          up = Face
                    White   White   White
                    White   White   White
                    Yellow  Yellow  Yellow,
          down = Face
                    White   White   White
                    Yellow  Yellow  Yellow
                    Yellow  Yellow  Yellow,
          right = Face
                    Orange Red Red
                    Orange Red Red
                    Orange Red Red,
          back = Face
                    Blue Blue Blue
                    Blue Blue Blue
                    Blue Blue Blue
        }

    it "right turn" $
      rightTurn startingCube `shouldBe`
        Cube {
          front =
            Face
              Green Green Yellow
              Green Green Yellow
              Green Green Yellow,
          left =
            Face
              Orange Orange Orange
              Orange Orange Orange
              Orange Orange Orange,
          up =
            Face
              White White Green
              White White Green
              White White Green,
          down =
            Face
              Yellow Yellow Blue
              Yellow Yellow Blue
              Yellow Yellow Blue,
          right =
            Face
              Red Red Red
              Red Red Red
              Red Red Red,
          back =
            Face
              White Blue Blue
              White Blue Blue
              White Blue Blue
        }

    it "4 right turns solves it" $
      solved ((rightTurn . rightTurn . rightTurn . rightTurn) startingCube) `shouldBe` True

    it "up turn" $
      upTurn startingCube `shouldBe`
        Cube {
          front =
            Face
              Red   Red   Red
              Green Green Green
              Green Green Green,
          left =
            Face
              Green   Green   Green
              Orange  Orange  Orange
              Orange  Orange  Orange,
          up =
            Face
              White White White
              White White White
              White White White,
          down =
            Face
              Yellow Yellow Yellow
              Yellow Yellow Yellow
              Yellow Yellow Yellow,
          right =
            Face
              Blue    Blue    Blue
              Red     Red     Red
              Red     Red     Red,
          back =
            Face
              Orange  Orange  Orange
              Blue    Blue    Blue
              Blue    Blue    Blue
        }

    it "4 up turns solves it" $
      solved ((upTurn . upTurn . upTurn . upTurn) startingCube) `shouldBe` True

    it "left turn" $
      leftTurn startingCube `shouldBe`
        Cube {
          front =
            Face
              White   Green   Green
              White   Green   Green
              White   Green   Green,
          left =
            Face
              Orange  Orange  Orange
              Orange  Orange  Orange
              Orange  Orange  Orange,
          up =
            Face
              Blue    White   White
              Blue    White   White
              Blue    White   White,
          down =
            Face
              Green   Yellow  Yellow
              Green   Yellow  Yellow
              Green   Yellow  Yellow,
          right =
            Face
              Red     Red     Red
              Red     Red     Red
              Red     Red     Red,
          back =
            Face
              Blue    Blue    Yellow
              Blue    Blue    Yellow
              Blue    Blue    Yellow
        }

    it "4 left turns solves it" $
      solved ((leftTurn . leftTurn . leftTurn . leftTurn) startingCube) `shouldBe` True

    it "back turn" $
      backTurn startingCube `shouldBe`
        Cube {
          front =
            Face
              Green   Green   Green
              Green   Green   Green
              Green   Green   Green,
          left =
            Face
              White   Orange  Orange
              White   Orange  Orange
              White   Orange  Orange,
          up =
            Face
              Red     Red     Red
              White   White   White
              White   White   White,
          down =
            Face
              Yellow  Yellow  Yellow
              Yellow  Yellow  Yellow
              Orange  Orange  Orange,
          right =
            Face
              Red     Red     Yellow
              Red     Red     Yellow
              Red     Red     Yellow,
          back =
            Face
              Blue    Blue    Blue
              Blue    Blue    Blue
              Blue    Blue    Blue
        }

    it "4 back turns solves it" $
      solved ((backTurn . backTurn . backTurn . backTurn) startingCube) `shouldBe` True

    it "down turn" $
      downTurn startingCube `shouldBe`
        Cube {
          front =
            Face
              Green   Green   Green
              Green   Green   Green
              Orange  Orange  Orange,
          left =
            Face
              Orange  Orange  Orange
              Orange  Orange  Orange
              Blue    Blue    Blue,
          up =
            Face
              White   White   White
              White   White   White
              White   White   White,
          down =
            Face
              Yellow  Yellow  Yellow
              Yellow  Yellow  Yellow
              Yellow  Yellow  Yellow,
          right =
            Face
              Red     Red     Red
              Red     Red     Red
              Green   Green   Green,
          back =
            Face
              Blue    Blue    Blue
              Blue    Blue    Blue
              Red     Red     Red
        }

    it "4 down turns solves it" $
      solved ((downTurn . downTurn . downTurn . downTurn) startingCube) `shouldBe` True

  describe "patterns" $
    it "checkerboard" $
      ((apply checkerboard) . (apply checkerboard)) startingCube `shouldBe` startingCube




