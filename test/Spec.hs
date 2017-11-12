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
              Orange Orange Orange
              Orange Orange Orange
              Orange Orange Orange,
          left =
            Face
              Green Green Green
              Green Green Green
              Green Green Green,
          up =
            Face
              Yellow Yellow Yellow
              Yellow Yellow Yellow
              Yellow Yellow Yellow,
          down =
            Face
              White White White
              White White White
              White White White,
          right =
            Face
              Blue Blue Blue
              Blue Blue Blue
              Blue Blue Blue,
          back =
            Face
              Red Red Red
              Red Red Red
              Red Red Red
        }
    it "new is solved" $
      solved startingCube `shouldBe` True
  describe "turns" $ do
    it "front" $
      frontTurn startingCube `shouldBe`
        Cube {
          front = Face
                    Orange Orange Orange
                    Orange Orange Orange
                    Orange Orange Orange,
          left = Face
                    Green Green White
                    Green Green White
                    Green Green White,
          up = Face
                    Yellow Yellow Yellow
                    Yellow Yellow Yellow
                    Green  Green  Green,
          down = Face
                    Blue  Blue  Blue
                    White White White
                    White White White,
          right = Face
                    Yellow Blue Blue
                    Yellow Blue Blue
                    Yellow Blue Blue,
          back = Face
                    Red Red Red
                    Red Red Red
                    Red Red Red
        }
    it "not solved if turned" $
      solved (frontTurn startingCube) `shouldBe` False


