module HanoiSpec where
    
import           Hanoi
import           Test.Hspec
    
spec :: Spec
spec = 
    describe "toDigits" $
        it "hanoi 2 \"a\" \"b\" \"c\" [(\"a\",\"c\"),(\"a\",\"b\"),(\"c\",\"b\")]" $ hanoi 2 "a" "b" "c" `shouldBe` [("a","c"),("a","b"),("c","b")]
            