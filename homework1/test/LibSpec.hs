module LibSpec where
    
import           Lib
import           Test.Hspec
    
spec :: Spec
spec = do
    describe "toDigits" $ do
        it "toDigits negative should be empty list" $ toDigits (-17) `shouldBe` []
        it "toDigits 0 should be empty list" $ toDigits 0 `shouldBe` []
        it "toDigits 1234 should be [1,2,3,4]" $ toDigits 1234 `shouldBe` [1,2,3,4]
        it "toDigits 1230 should be [1,2,3,0]" $ toDigits 1230 `shouldBe` [1,2,3,0]
    describe "toDigitsRev" $ do
        it "toDigitsRev negative should be empty list" $ toDigitsRev (-17) `shouldBe` []
        it "toDigitsRev 0 should be empty list" $ toDigitsRev 0 `shouldBe` []
        it "toDigitsRev 1234 should be [4,3,2,1]" $ toDigitsRev 1234 `shouldBe` [4,3,2,1]
        it "toDigitsRev 1230 should be [0,3,2,1]" $ toDigitsRev 1230 `shouldBe` [0,3,2,1]
    describe "doubleEveryOther" $ do
        it "doubleEveryOther [8,7,6,5] should be [16,7,12,5]" $ doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
        it "doubleEveryOther [1,2,3] should be [1,4,3]" $ doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
    describe "sumDigits" $ do
        it "sumDigits [1,2,3] should be 6" $ sumDigits [1,2,3] `shouldBe` 6
        it "sumDigits [11,22,33] should be 12" $ sumDigits [11,22,33] `shouldBe` 12
    describe "validate" $ do
        it "validate 4012888888881881 should be True" $ validate 4012888888881881 `shouldBe` True
        it "validate 4012888888881882 should be False" $ validate 4012888888881882 `shouldBe` False
            