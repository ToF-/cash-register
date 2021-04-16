module RegisterSpec
    where
import Test.Hspec
import Control.Monad.Writer (writer, runWriter)
import Register
spec :: SpecWith ()
spec = do
    describe "register" $ do
        it "can input a single price" $ do
            let out = \s -> writer ((), s)
                inp = return "42"
                run = register inp out
                result = lines (snd (runWriter run))
            result `shouldBe` ["42.00"]

    describe "money" $ do
        it "can be read and shown" $ do
            showMoney (readMoney "42")  `shouldBe` "42.00"
            showMoney (readMoney "4.7") `shouldBe` "4.70"
            showMoney (readMoney "14.") `shouldBe` "14.00"
            showMoney (readMoney "13.02") `shouldBe` "13.02"
            showMoney (readMoney ".2") `shouldBe` "0.20"
            showMoney (readMoney ".07") `shouldBe` "0.07"

