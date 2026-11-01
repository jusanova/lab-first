module Lab.CapitalizeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Lab.Capitalize

spec :: Spec
spec = do
  it "pārveido parastu teikumu" $
    capitalizeSentence "katra teikuma vārda pirmo burtu"
      `shouldBe` "Katra Teikuma Vārda Pirmo Burtu"
  it "pārveido latviešu sākumburtus" $
    capitalizeSentence "ēna ūdens šis" `shouldBe` "Ēna Ūdens Šis"
  it "noņem liekās atstarpes un tabulācijas" $
    capitalizeSentence "  viens   divi\ttrīs  " `shouldBe` "Viens Divi Trīs"
  it "nemaina saīsinājumus un vārdus ar ciparu sākumā" $
    capitalizeSentence "NASA un 3d" `shouldBe` "NASA Un 3d"
  prop "sakrīt ar unwords . map capitalizeWord . words" $ \s ->
    capitalizeSentence s `shouldBe` unwords (map capitalizeWord (words s))
