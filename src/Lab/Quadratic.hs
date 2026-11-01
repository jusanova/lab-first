{- |
Modulis  : Lab.Quadratic
Uzdevums : 4. uzdevums, 3. variants
Apraksts : Funkcija 'numbOfSol' nosaka vienādojuma h·x² + i·x + j = 0
           reālo atrisinājumu skaitu ar gadījumu izšķiršanu.

== Funkciju tipi (GHCi komanda @:type@ jeb @:t@)

@
tail      :: [a] -> [a]
sqrt      :: Floating a => a -> a
pi        :: Floating a => a
exp       :: Floating a => a -> a
(^)       :: (Num a, Integral b) => a -> b -> a
(/=)      :: Eq a => a -> a -> Bool
numbOfSol :: (Ord a, Num a) => a -> a -> a -> SolCount
@

Tipu programmā norāda ar tipa signatūru (@f :: T@) pirms definīcijas vai ar
anotāciju izteiksmē, piemēram @(read "5" :: Double)@.
-}
module Lab.Quadratic
  ( SolCount (..)
  , numbOfSol
  , describeSol
  , showRational
  ) where

import Data.Ratio (denominator, numerator)

-- | Atrisinājumu skaits. Atsevišķs tips vajadzīgs, jo identitātei 0 = 0
-- atrisinājumu ir bezgalīgi daudz, ko nevar korekti attēlot ar 'Int'.
data SolCount
  = Finite Int   -- ^ galīgs skaits: 0, 1 vai 2
  | Infinite     -- ^ der jebkurš x (h = i = j = 0)
  deriving (Eq, Show)

-- | Nosaka vienādojuma @h·x² + i·x + j = 0@ reālo atrisinājumu skaitu.
--
-- Parametri:
--
--   * @h@ – koeficients pie x²;
--   * @i@ – koeficients pie x;
--   * @j@ – brīvais loceklis.
--
-- Atgriež: 'SolCount'.
--
-- Gadījumi:
--
--   * @h /= 0@ – kvadrātvienādojums; nosaka diskriminanta @D = i² − 4hj@
--     zīme: D > 0 → 2, D = 0 → 1, D < 0 → 0;
--   * @h == 0, i /= 0@ – lineārs vienādojums, 1 atrisinājums;
--   * @h == 0, i == 0, j /= 0@ – pretruna (piem. 5 = 0), 0 atrisinājumu;
--   * @h == i == j == 0@ – identitāte 0 = 0, bezgalīgi daudz.
--
-- Kvadrātsakne nav vajadzīga, tāpēc pietiek ar @(Ord a, Num a)@, un funkciju
-- var izsaukt ar precīziem tipiem ('Integer', 'Rational'). Ar 'Double'
-- vienādojums x² + 0.2x + 0.01 = 0 noapaļošanas dēļ dod 2, nevis 1.
--
-- >>> numbOfSol 1 (-2) (1 :: Integer)
-- Finite 1
numbOfSol :: (Ord a, Num a) => a -> a -> a -> SolCount
numbOfSol h i j
  | h /= 0 && d > 0  = Finite 2
  | h /= 0 && d == 0 = Finite 1
  | h /= 0           = Finite 0
  | i /= 0           = Finite 1
  | j /= 0           = Finite 0
  | otherwise        = Infinite
  where
    d = i ^ (2 :: Int) - 4 * h * j

-- | Pārvērš rezultātu teikumā.
describeSol :: SolCount -> String
describeSol Infinite   = "Vienādojumam ir bezgalīgi daudz atrisinājumu (0 = 0)."
describeSol (Finite 0) = "Vienādojumam nav reālu atrisinājumu."
describeSol (Finite 1) = "Vienādojumam ir 1 reāls atrisinājums."
describeSol (Finite n) = "Vienādojumam ir " ++ show n ++ " reāli atrisinājumi."

-- | Racionāla skaitļa izdruka: veseliem bez daļas (@3@), citiem kā @p/q@.
showRational :: Rational -> String
showRational r
  | denominator r == 1 = show (numerator r)
  | otherwise          = show (numerator r) ++ "/" ++ show (denominator r)
