{- |
Modulis  : Lab.RemoveAll
Uzdevums : 6. uzdevums, 3. variants
Apraksts : Izņem no pirmā saraksta visus otrā saraksta elementus:
           @removeAll [1, 2, 3, 2, 4] [2, 4] == [1, 3]@.

Atšķirība no @(\\\\)@ no "Data.List": tas katram otrā saraksta elementam
izņem tikai /pirmo/ sakritību, bet šeit tiek izņemti /visi/ atkārtojumi.
-}
module Lab.RemoveAll
  ( removeAll
  , removeAllOrd
  ) where

import Data.List (sort, sortBy)
import Data.Ord (comparing)

-- | Izņem no @xs@ visus elementus, kas ir sarakstā @ys@.
--
-- Parametri:
--
--   * @xs@ – saraksts, no kura elementi tiek izņemti;
--   * @ys@ – izņemamie elementi.
--
-- Atgriež: @xs@ elementus, kuru nav @ys@, sākotnējā secībā.
-- Sarežģītība O(n·m). Slinka pēc @xs@, tāpēc @xs@ var būt bezgalīgs.
--
-- >>> removeAll [1, 2, 3, 2, 4] [2, 4]
-- [1,3]
removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs ys = filter (`notElem` ys) xs

-- | Ātrāka versija elementiem, kurus var sakārtot ('Ord').
--
-- Parametri un rezultāts – kā 'removeAll'.
--
-- Algoritms:
--
--   1. katram @xs@ elementam pievieno pozīciju un sakārto pēc vērtības;
--   2. sakārto @ys@;
--   3. sapludina (merge) abus sakārtotos sarakstus, izmetot sakritības;
--   4. atlikušos elementus sakārto atpakaļ pēc pozīcijas.
--
-- Sarežģītība O((n + m) log(n + m)). Darbojas tikai ar galīgu @xs@.
removeAllOrd :: Ord a => [a] -> [a] -> [a]
removeAllOrd xs ys = map snd (sortBy (comparing fst) kept)
  where
    indexed = sortBy (comparing snd) (zip [0 :: Int ..] xs)
    kept    = mergeOut indexed (sort ys)

    mergeOut [] _ = []
    mergeOut ps [] = ps
    mergeOut ps@((i, x) : rest) bs@(b : bs')
      | x < b     = (i, x) : mergeOut rest bs
      | x == b    = mergeOut rest bs   -- b paliek: x var atkārtoties
      | otherwise = mergeOut ps bs'
