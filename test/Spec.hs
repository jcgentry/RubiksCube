import Test.Hspec
import Cube

main :: IO ()
main = hspec $
  describe "Cube" $ do
    it "new cube" $
      startingCube `shouldBe`
        Cube
            (Face Orange Orange Orange Orange Orange Orange Orange Orange Orange)
            (Face Green Green Green Green Green Green Green Green Green)
            (Face Yellow Yellow Yellow Yellow Yellow Yellow Yellow Yellow Yellow)
            (Face White White White White White White White White White)
            (Face Blue Blue Blue Blue Blue Blue Blue Blue Blue)
            (Face Red Red Red Red Red Red Red Red Red)
    it "new is solved" $
      solved startingCube `shouldBe` True