module Lab.RemoveAllSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Lab.Parse (parseIntegers)
import Lab.RemoveAll

spec :: Spec
spec = do
  it "izņem elementus, saglabājot secību" $
    removeAll [1, 2, 3, 2, 4] [2, 4 :: Int] `shouldBe` [1, 3]
  it "izņem visus atkārtojumus, ne tikai pirmo" $
    removeAll [2, 1, 2, 3, 2] [2 :: Int] `shouldBe` [1, 3]
  it "tukšs otrais saraksts neko nemaina" $
    removeAll [1, 2, 3] ([] :: [Int]) `shouldBe` [1, 2, 3]
  it "noraida burtu skaitļa vietā" $
    parseIntegers "1, x, 3" `shouldSatisfy` isLeft
  prop "ātrā versija removeAllOrd sakrīt ar removeAll" $ \xs ys ->
    removeAllOrd xs ys `shouldBe` removeAll xs (ys :: [Int])
