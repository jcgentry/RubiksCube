import Test.Hspec
import Cube

main :: IO ()
main = hspec $ do
  describe "Cube" $ do
    it "new cube" $
      startingCube `shouldBe`
        Cube {
          front =
            Face
              Green Green Green
              Green Green Green
              Green Green Green,
          left =
            Face
              Orange Orange Orange
              Orange Orange Orange
              Orange Orange Orange,
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
              Red Red Red
              Red Red Red
              Red Red Red,
          back =
            Face
              Blue Blue Blue
              Blue Blue Blue
              Blue Blue Blue
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



