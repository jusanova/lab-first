{- |
Modulis  : Lab.ReverseWord
Uzdevums : 2. uzdevums, 3. variants (1. risinājums)
Apraksts : Apgriež vārda simbolus trīs soļos:
             1) sadala virkni rakstzīmju masīvā;
             2) sakārto to apgrieztā secībā;
             3) apvieno rakstzīmes atpakaļ.

Haskell valodā 'String' jau ir rakstzīmju saraksts @[Char]@, tāpēc 1. solis
pārvērš virkni viena simbola virkņu sarakstā, lai katrs solis būtu redzams.
-}
module Lab.ReverseWord
  ( splitChars
  , reverseList
  , joinChars
  , reverseWord
  , isPalindrome
  , InputCheck (..)
  , checkInput
  , describeReverse
  ) where

import Data.List (intercalate)

-- | 1. solis – sadala virkni atsevišķu rakstzīmju masīvā.
--
-- >>> splitChars "abc"
-- ["a","b","c"]
splitChars :: String -> [String]
splitChars = map (: [])

-- | 2. solis – apgriež saraksta secību.
--
-- Parametri:
--
--   * @xs@ – jebkura tipa saraksts.
--
-- Atgriež: to pašu sarakstu pretējā secībā. Realizēts ar @foldl@ un
-- akumulatoru: katrs elements tiek likts akumulatora sākumā, tāpēc
-- sarežģītība ir O(n), nevis O(n²), kā lietojot @++@ katrā solī.
--
-- >>> reverseList [1, 2, 3]
-- [3,2,1]
reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

-- | 3. solis – apvieno viena simbola virknes atpakaļ vienā virknē.
joinChars :: [String] -> String
joinChars = concat

-- | Apgriež vārdu, izpildot visus trīs soļus pēc kārtas.
--
-- >>> reverseWord "Haskell"
-- "lleksaH"
reverseWord :: String -> String
reverseWord = joinChars . reverseList . splitChars

-- | Pārbauda, vai vārds sakrīt ar savu apgriezto versiju.
isPalindrome :: String -> Bool
isPalindrome w = w == reverseWord w

-- | Lietotāja ievades pārbaudes rezultāts.
data InputCheck
  = EmptyInput             -- ^ ievadītas tikai atstarpes vai nekas
  | ValidWord String       -- ^ viens vārds (bez malu atstarpēm)
  | SeveralWords [String]  -- ^ vairāki vārdi
  deriving (Eq, Show)

-- | Pārbauda lietotāja ievadi pirms apgriešanas.
--
-- Parametri:
--
--   * @input@ – neapstrādāta ievades rinda.
--
-- Atgriež: 'EmptyInput', 'ValidWord' vai 'SeveralWords'.
checkInput :: String -> InputCheck
checkInput input = case words input of
  []  -> EmptyInput
  [w] -> ValidWord w
  ws  -> SeveralWords ws

-- | Izveido atbildes tekstu lietotājam.
--
-- Parametri:
--
--   * @input@ – neapstrādāta ievades rinda.
--
-- Atgriež: apgriezto vārdu (vai katru vārdu atsevišķi) ar piezīmi par
-- palindromu, vai kļūdas tekstu tukšai ievadei.
describeReverse :: String -> String
describeReverse input = case checkInput input of
  EmptyInput      -> "Kļūda: nav ievadīts neviens vārds."
  ValidWord w     -> describeOne w
  SeveralWords ws ->
    "Ievadīti " ++ show (length ws) ++ " vārdi, apgriežu katru atsevišķi:\n"
      ++ intercalate "\n" (map (("  " ++) . describeOne) ws)
  where
    describeOne w =
      "Vārda \"" ++ w ++ "\" apgrieztā versija ir \"" ++ reverseWord w ++ "\""
        ++ (if isPalindrome w then " (vārds ir palindroms)." else ".")
