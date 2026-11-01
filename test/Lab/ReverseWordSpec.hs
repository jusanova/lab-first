module Lab.ReverseWordSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Lab.ReverseWord

spec :: Spec
spec = do
  it "apgriež parastu vārdu" $
    reverseWord "Haskell" `shouldBe` "lleksaH"
  it "apgriež vārdu ar latviešu burtiem" $
    reverseWord "ābols" `shouldBe` "slobā"
  it "tukšu virkni atstāj tukšu" $
    reverseWord "" `shouldBe` ""
  it "ievade tikai ar atstarpēm ir tukša" $
    checkInput "    " `shouldBe` EmptyInput
  prop "divreiz apgriezts vārds ir sākotnējais" $ \s ->
    reverseWord (reverseWord s) `shouldBe` s
