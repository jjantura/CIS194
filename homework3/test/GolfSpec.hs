module GolfSpec where

import           Golf
import           Test.Hspec
    
spec :: SpecWith()        
spec = do
    describe "skips" $ do
        it "test 1" $ skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
        it "test 2" $ skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
        it "test 3" $ skips [1] `shouldBe` [[1]]
        it "test 4" $ skips [True, False] `shouldBe` [[True,False], [False]]
        it "test 5" $ skips [] `shouldBe` []
