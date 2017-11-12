import Test.Hspec
import Cube

main :: IO ()
main = hspec $ do
  describe "Cube" $ do
    it "new cube" $ do
      startingCube `shouldBe`
        Cube {
          f = Face Orange Orange Orange Orange Orange Orange Orange Orange Orange,
          l = Face Green Green Green Green Green Green Green Green Green,
          u = Face Yellow Yellow Yellow Yellow Yellow Yellow Yellow Yellow Yellow,
          d = Face White White White White White White White White White,
          r = Face Blue Blue Blue Blue Blue Blue Blue Blue Blue,
          b = Face Red Red Red Red Red Red Red Red Red
        }