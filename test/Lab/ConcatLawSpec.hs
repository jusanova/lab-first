module Lab.ConcatLawSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Fun (..))

import Lab.ConcatLaw

spec :: Spec
spec = do
  prop "likums izpildās nejaušai funkcijai Int -> Int" $ \(Fun _ f) xss ->
    lhs f xss `shouldBe` rhs (f :: Int -> Int) (xss :: [[Int]])
  prop "likums izpildās funkcijai, kas maina tipu (Int -> Bool)" $ \(Fun _ f) xss ->
    lhs f xss `shouldBe` rhs (f :: Int -> Bool) (xss :: [[Int]])
  it "likums izpildās bezgalīgam sarakstam (pirmie 20 elementi)" $
    take 20 (lhs (* 2) infinite) `shouldBe` take 20 (rhs (* 2) infinite)
  prop "lemma: map f (xs ++ ys) = map f xs ++ map f ys" $ \(Fun _ f) xs ys ->
    map f (xs ++ ys) `shouldBe` map (f :: Int -> Int) xs ++ map f ys
  prop "visi induktīvā soļa starprezultāti ir vienādi" $ \(Fun _ f) xs xss ->
    allEqual (map snd (proofSteps (f :: Int -> Int) xs xss)) `shouldBe` True
  where
    infinite = map (\n -> [n, n + 1]) [1 ..] :: [[Int]]
    allEqual vs = and (zipWith (==) vs (drop 1 vs))
