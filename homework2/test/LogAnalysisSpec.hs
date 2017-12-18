module LogAnalysisSpec where

import           Log
import           LogAnalysis
import           Test.Hspec

spec :: SpecWith()        
spec = 
    describe "parseMessage" $ do
        it "should return valid info message" $ parseMessage "I 123 text" `shouldBe` LogMessage Info 123 "text"
        it "should return valid warning message" $ parseMessage "W 123 text" `shouldBe` LogMessage Warning 123 "text"
        it "should return valid error message" $ parseMessage "E 100 123 text" `shouldBe` LogMessage (Error 100) 123 "text"

        it "should return Unknown for plain string" $ parseMessage "The Magic Words are Squeamish Ossifrage" `shouldBe` Unknown "The Magic Words are Squeamish Ossifrage"
        it "should return Unknown for invalid timestamp" $ parseMessage "I TS somestring" `shouldBe` Unknown "I TS somestring"
        it "should return Unknown for empty string" $ parseMessage "" `shouldBe` Unknown ""
        it "should return Unknown for invalid error level" $ parseMessage "E EL 120 meh" `shouldBe` Unknown "E EL 120 meh"