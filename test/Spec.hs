import Test.Hspec
import Cube

main :: IO ()
main = hspec $ do
  describe "Cube" $ do
    it "new cube" $
      startingCube `shouldBe`
        Cube
            (Face
              Orange Orange Orange
              Orange Orange Orange
              Orange Orange Orange)
            (Face
              Green Green Green
              Green Green Green
              Green Green Green)
            (Face
              Yellow Yellow Yellow
              Yellow Yellow Yellow
              Yellow Yellow Yellow)
            (Face
              White White White
              White White White
              White White White)
            (Face
              Blue Blue Blue
              Blue Blue Blue
              Blue Blue Blue)
            (Face
              Red Red Red
              Red Red Red
              Red Red Red)
    it "new is solved" $
      solved startingCube `shouldBe` True
  describe "turns" $ do
    it "front" $
      fTurn startingCube `shouldBe`
        Cube {
          f = Face
                    Orange Orange Orange
                    Orange Orange Orange
                    Orange Orange Orange,
          l = Face
                    Green Green White
                    Green Green White
                    Green Green White,
          u = Face
                    Yellow Yellow Yellow
                    Yellow Yellow Yellow
                    Green  Green  Green,
          d = Face
                    Blue  Blue  Blue
                    White White White
                    White White White,
          r = Face
                    Yellow Blue Blue
                    Yellow Blue Blue
                    Yellow Blue Blue,
          b = Face
                    Red Red Red
                    Red Red Red
                    Red Red Red
        }
    it "not solved if turned" $
      solved (fTurn startingCube) `shouldBe` False


