module LogAnalysisSpec where

import           Log
import           LogAnalysis
import           Test.Hspec
        
spec = 
    describe "parseMessage" $ do
        it "should return valid info message" $ parseMessage "I 123 text" `shouldBe` LogMessage Info 123 "text"
        it "should return valid warning message" $ parseMessage "W 123 text" `shouldBe` LogMessage Warning 123 "text"
        it "should return valid error message" $ parseMessage "E 100 123 text" `shouldBe` LogMessage (Error 100) 123 "text"
                                