import Test.Hspec
import Cube
import Pattern
import Turn

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
      applyTurn F startingCube `shouldBe`
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
      solved (applyTurn F startingCube) `shouldBe` False

    it "4 front turns solves it" $
      solved (applyPattern (Pattern [F, F, F, F]) startingCube) `shouldBe` True

    it "two front turns" $
      applyPattern (Pattern [F, F]) startingCube `shouldBe`
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
      applyTurn R startingCube `shouldBe`
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
      solved (applyPattern (Pattern [R, R, R, R]) startingCube) `shouldBe` True

    it "up turn" $
      applyTurn U startingCube `shouldBe`
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
      solved (applyPattern (Pattern [U, U, U, U]) startingCube) `shouldBe` True

    it "left turn" $
      applyTurn L startingCube `shouldBe`
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
      solved (applyPattern (Pattern [L, L, L, L]) startingCube) `shouldBe` True

    it "back turn" $
      applyTurn B startingCube `shouldBe`
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
      solved (applyPattern (Pattern [B, B, B, B]) startingCube) `shouldBe` True

    it "down turn" $
      applyTurn D startingCube `shouldBe`
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
      solved (applyPattern (Pattern [D, D, D, D]) startingCube) `shouldBe` True

  describe "patterns" $
    it "checkerboard" $
      applyPattern (combine checkerboard checkerboard) startingCube `shouldBe` startingCube




