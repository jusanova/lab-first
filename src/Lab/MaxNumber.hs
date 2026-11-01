{- |
Modulis  : Lab.MaxNumber
Uzdevums : 1. uzdevums, 3. variants (1. risinājums)
Apraksts : Nosaka lielāko skaitli sarakstā: sakārto sarakstu dilstošā
           secībā un paņem pirmo elementu.
-}
module Lab.MaxNumber
  ( sortDesc
  , findMax
  , describeMax
  ) where

import Data.List (sortBy)

-- | Sakārto sarakstu dilstošā secībā.
--
-- Parametri:
--
--   * @xs@ – saraksts, kura elementus var salīdzināt ('Ord').
--
-- Atgriež: tos pašus elementus no lielākā uz mazāko.
-- Sarežģītība O(n log n).
--
-- >>> sortDesc [5, 10, 11, 15, 3, 8]
-- [15,11,10,8,5,3]
sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

-- | Atrod lielāko elementu: sakārto dilstoši un paņem pirmo.
--
-- Parametri:
--
--   * @xs@ – skaitļu (vai citu salīdzināmu vērtību) saraksts.
--
-- Atgriež: @Just m@, kur @m@ ir lielākais elements, vai 'Nothing' tukšam
-- sarakstam. 'Maybe' nozīmē, ka tukšs saraksts neizraisa izpildes kļūdu,
-- kā tas notiek ar standarta 'maximum'.
--
-- >>> findMax [5, 10, 11, 15, 3, 8]
-- Just 15
findMax :: Ord a => [a] -> Maybe a
findMax xs = case sortDesc xs of
  []      -> Nothing
  (m : _) -> Just m

-- | Izveido lietotājam saprotamu paziņojumu par maksimālo skaitli.
--
-- Parametri:
--
--   * @xs@ – skaitļu saraksts.
--
-- Atgriež: “Maksimālais skaitlis ir X.” vai paskaidrojumu, ka tukšam
-- sarakstam maksimumu noteikt nevar.
describeMax :: (Ord a, Show a) => [a] -> String
describeMax xs = case findMax xs of
  Nothing -> "Saraksts ir tukšs – maksimālo skaitli nevar noteikt."
  Just m  -> "Maksimālais skaitlis ir " ++ show m ++ "."
