module LogAnalysisSpec where

import           Log
import           LogAnalysis
import           Test.Hspec
        
spec :: Spec
spec = do
    describe "toDigits" $ do
        parseMessage "I 123 text" `shouldBe` LogMessage Info 123 "text"