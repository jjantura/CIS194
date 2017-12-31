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
--  TODO: verify why it works in ghci but not here
--        it "test 5" $ skips [] `shouldBe` []
    describe "localMaxima" $ do
        it "test 1" $ localMaxima [2,9,5,6,1] `shouldBe` [9,6]
        it "test 2" $ localMaxima [2,3,4,1,5] `shouldBe` [4]
        it "test 3" $ localMaxima [1,2,3,4,5] `shouldBe` []
    describe "histogram" $ do
        it "test 1" $ histogram [1,1,1,5] `shouldBe` "\n *        \n *        \n *   *    \n==========\n0123456789\n"
        it "test 2" $ histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` "\n    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"