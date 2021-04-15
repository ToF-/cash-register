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
