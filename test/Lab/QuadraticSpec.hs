module Lab.QuadraticSpec (spec) where

import Data.Ratio ((%))
import Test.Hspec

import Lab.Quadratic

spec :: Spec
spec = do
  it "D > 0: x² − 5x + 6 = 0 ir 2 atrisinājumi" $
    numbOfSol 1 (-5) (6 :: Integer) `shouldBe` Finite 2
  it "D = 0: x² + 0.2x + 0.01 = 0 ir 1 atrisinājums (precīzi ar Rational)" $
    numbOfSol 1 (1 % 5) (1 % 100 :: Rational) `shouldBe` Finite 1
  it "D < 0: x² + 1 = 0 nav atrisinājumu" $
    numbOfSol 1 0 (1 :: Integer) `shouldBe` Finite 0
  it "h = 0: lineārs 3x + 6 = 0 ir 1 atrisinājums" $
    numbOfSol 0 3 (6 :: Integer) `shouldBe` Finite 1
  it "h = i = j = 0: bezgalīgi daudz atrisinājumu" $
    numbOfSol 0 0 (0 :: Integer) `shouldBe` Infinite
