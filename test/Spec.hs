{- |
Modulis  : Main (testu komplekts)
Apraksts : Palaiž visu uzdevumu testus. @cabal test@ beidzas ar kodu 1,
           ja kaut viens tests neizdodas – to izmanto CI.
-}
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (hSetEncoding, stderr, stdout)
import Test.Hspec (hspec, describe)

import qualified Lab.CapitalizeSpec
import qualified Lab.ConcatLawSpec
import qualified Lab.ConcatSpec
import qualified Lab.MaxNumberSpec
import qualified Lab.QuadraticSpec
import qualified Lab.RemoveAllSpec
import qualified Lab.ReverseWordSpec
import qualified Lab.SearchTreeSpec

main :: IO ()
main = do
  -- Testu nosaukumos ir simboli ārpus Windows konsoles kodējuma.
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hspec $ do
    describe "1. uzdevums – maksimālais skaitlis" Lab.MaxNumberSpec.spec
    describe "2. uzdevums – vārda apgriešana"     Lab.ReverseWordSpec.spec
    describe "3. uzdevums – lieli sākumburti"     Lab.CapitalizeSpec.spec
    describe "4. uzdevums – numbOfSol"            Lab.QuadraticSpec.spec
    describe "5. uzdevums – concat"               Lab.ConcatSpec.spec
    describe "6. uzdevums – removeAll"            Lab.RemoveAllSpec.spec
    describe "7. uzdevums – meklēšanas koks"      Lab.SearchTreeSpec.spec
    describe "8. uzdevums – map/concat likums"    Lab.ConcatLawSpec.spec
