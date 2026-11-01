{- |
7. uzdevuma testi: 'isSearchTree', īpašība “ievietošana saglabā BMK” un
eksperiments ar kļūdainām 'insertTree' versijām.

Kļūdainajām versijām tiek lietots 'expectFailure': tests izdodas tikai tad,
ja QuickCheck atrod pretpiemēru, t.i., īpašība kļūdu atklāj.
-}
module Lab.SearchTreeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, choose, expectFailure, forAll, listOf)

import Lab.SearchTree

spec :: Spec
spec = do
  it "atrod kļūdu pret senci, ne tikai pret vecāku" $
    isSearchTree (Branch 5 (Branch 3 Leaf (node 6)) (node (8 :: Int))) `shouldBe` False
  it "aizliedz dublikātu labajā apakškokā" $
    isSearchTree (Branch 4 Leaf (node 4) :: Tree Int) `shouldBe` False
  prop "ievietošana saglabā BMK" $
    keepsSearchTree insertTree
  prop "īpašība atklāj kļūdu: samainītas salīdzinājuma zīmes" $
    expectFailure (keepsSearchTree insertSwapped)
  prop "īpašība atklāj kļūdu: dublikāti pa labi" $
    expectFailure (keepsSearchTree insertDupRight)

-- | Īpašība: ja koks ir BMK, tad pēc @ins@ izsaukuma tas joprojām ir BMK.
-- Koks tiek būvēts ar pareizo 'insertTree', tāpēc priekšnosacījums vienmēr
-- izpildās. Vērtības ir 0..9, lai bieži rastos dublikāti.
keepsSearchTree :: (Int -> Tree Int -> Tree Int) -> Property
keepsSearchTree ins =
  forAll (listOf smallInt) $ \xs ->
    forAll smallInt $ \e ->
      isSearchTree (ins e (fromList xs))
  where
    smallInt :: Gen Int
    smallInt = choose (0, 9)

node :: a -> Tree a
node x = Branch x Leaf Leaf

-- | Kļūdaina versija: salīdzinājuma zīmes samainītas vietām.
insertSwapped :: Ord a => a -> Tree a -> Tree a
insertSwapped e Leaf = Branch e Leaf Leaf
insertSwapped e (Branch x li re)
  | e > x     = Branch x (insertSwapped e li) re
  | otherwise = Branch x li (insertSwapped e re)

-- | Kļūdaina versija: vienādas vērtības tiek liktas pa labi.
insertDupRight :: Ord a => a -> Tree a -> Tree a
insertDupRight e Leaf = Branch e Leaf Leaf
insertDupRight e (Branch x li re)
  | e < x     = Branch x (insertDupRight e li) re
  | otherwise = Branch x li (insertDupRight e re)
