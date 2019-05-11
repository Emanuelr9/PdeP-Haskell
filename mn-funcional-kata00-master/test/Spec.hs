import Lib
import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Test valores limite" $ do
    it "f 5" $ do
        (f 5) `shouldBe` 4
    
    it "f 6" $ do
       (f 6) `shouldBe` 6

    it "f 4" $ do
       (f 4) `shouldBe` 3

    it "f 0" $ do
       (f 0) `shouldBe` (-1)
    
    it "f -1" $ do
       (f (-1)) `shouldBe` 1
