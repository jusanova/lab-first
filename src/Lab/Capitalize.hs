{- |
Modulis  : Lab.Capitalize
Uzdevums : 3. uzdevums, 3. variants (2. risinājums)
Apraksts : Pārveido katra teikuma vārda pirmo burtu par lielo burtu ar
           reducēšanas (locīšanas, fold) funkciju: katram vārdam teikumam
           tiek pievienots atbilstošais pārveidotais vārds.
-}
module Lab.Capitalize
  ( capitalizeWord
  , addWord
  , capitalizeSentence
  ) where

import Data.Char (toUpper)

-- | Pārveido vārda pirmo simbolu par lielo burtu.
--
-- Parametri:
--
--   * @w@ – vārds.
--
-- Atgriež: vārdu ar lielo sākumburtu; pārējie simboli netiek mainīti, tāpēc
-- saīsinājumi (NASA) paliek nemainīti. Ja pirmais simbols nav burts
-- (cipars, pēdiņa), vārds nemainās.
--
-- >>> capitalizeWord "rīga"
-- "Rīga"
capitalizeWord :: String -> String
capitalizeWord []       = []
capitalizeWord (c : cs) = toUpper c : cs

-- | Reducēšanas solis: teikumam pievieno nākamo pārveidoto vārdu.
--
-- Parametri:
--
--   * @w@ – kārtējais vārds;
--   * @sentence@ – jau uzbūvētā teikuma daļa (akumulators).
--
-- Atgriež: teikumu ar pievienotu vārdu, atdalītu ar vienu atstarpi.
addWord :: String -> String -> String
addWord w sentence
  | null sentence = capitalizeWord w
  | otherwise     = capitalizeWord w ++ " " ++ sentence

-- | Pārveido katra teikuma vārda pirmo burtu par lielo burtu.
--
-- Parametri:
--
--   * @sentence@ – teikums.
--
-- Atgriež: teikumu, kurā katrs vārds sākas ar lielo burtu; liekās atstarpes
-- un tabulācijas tiek aizstātas ar vienu atstarpi.
--
-- Reducēšana notiek ar @foldr@: katrs vārds tiek pievienots akumulatora
-- sākumā (O(1)), tāpēc kopējā sarežģītība ir O(n). Variants ar @foldl@ un
-- @acc ++ " " ++ w@ būtu O(n²).
--
-- >>> capitalizeSentence "labdien, šodien ir saulaina diena"
-- "Labdien, Šodien Ir Saulaina Diena"
capitalizeSentence :: String -> String
capitalizeSentence = foldr addWord "" . words
