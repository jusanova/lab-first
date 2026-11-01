module Lab.MaxNumberSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonEmptyList (..))

import Lab.MaxNumber
import Lab.Parse (parseIntegers)

spec :: Spec
spec = do
  it "atrod maksimumu uzdevuma sarakstā" $
    describeMax [5, 10, 11, 15, 3, 8 :: Integer] `shouldBe` "Maksimālais skaitlis ir 15."
  it "tukšam sarakstam atgriež Nothing" $
    findMax ([] :: [Integer]) `shouldBe` Nothing
  it "strādā tikai ar negatīviem skaitļiem" $
    findMax [-5, -1, -9 :: Integer] `shouldBe` Just (-1)
  it "noraida burtu skaitļa vietā" $
    parseIntegers "5, abc, 7" `shouldSatisfy` isLeft
  prop "sakrīt ar standarta maximum" $ \(NonEmpty xs) ->
    findMax (xs :: [Int]) `shouldBe` Just (maximum xs)
