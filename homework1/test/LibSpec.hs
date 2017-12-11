module LibSpec where
    
import           Lib
import           Test.Hspec
    
spec :: Spec
spec = do
    describe "toDigits" $ do
        it "negative should be empty list" $ toDigits (-17) `shouldBe` []
        it "zero should be empty list" $ toDigits 0 `shouldBe` []
        it "1234 should be [1,2,3,4]" $ toDigits 1234 `shouldBe` [1,2,3,4]
        it "1230 should be [1,2,3,0]" $ toDigits 1230 `shouldBe` [1,2,3,0]
    describe "toDigitsRev" $ do
        it "negative should be empty list" $ toDigitsRev (-17) `shouldBe` []
        it "zero should be empty list" $ toDigitsRev 0 `shouldBe` []
        it "1234 should be [4,3,2,1]" $ toDigitsRev 1234 `shouldBe` [4,3,2,1]
        it "1230 should be [0,3,2,1]" $ toDigitsRev 1230 `shouldBe` [0,3,2,1]
    describe "doubleEveryOther" $ do
        it "[8,7,6,5] should be [16,7,12,5]" $ doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
        it "[1,2,3] should be [1,4,3]" $ doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
        