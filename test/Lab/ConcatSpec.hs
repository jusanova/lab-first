module Lab.ConcatSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified Lab.Concat as C

spec :: Spec
spec = do
  it "saplacina uzdevuma piemēru" $
    C.concat [[1, 2], [3], [], [4, 5 :: Int]] `shouldBe` [1, 2, 3, 4, 5]
  it "tikai tukši iekšējie saraksti dod tukšu sarakstu" $
    C.concat ([[], [], []] :: [[Int]]) `shouldBe` []
  it "saplacina virknes" $
    C.concat ["Haskell", " ir", " forss"] `shouldBe` "Haskell ir forss"
  it "ir slinka – darbojas ar bezgalīgu sarakstu" $
    take 5 (C.concat (repeat [1, 2 :: Int])) `shouldBe` [1, 2, 1, 2, 1]
  prop "sakrīt ar Prelude.concat" $ \xss ->
    C.concat xss `shouldBe` concat (xss :: [[Int]])
