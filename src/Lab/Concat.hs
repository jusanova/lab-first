{- |
Modulis  : Lab.Concat
Uzdevums : 5. uzdevums, 3. variants
Apraksts : Funkcija @concat :: [[a]] -> [a]@ saplacina sarakstu sarakstu:
           @concat [[1, 2], [3], [], [4, 5]] == [1, 2, 3, 4, 5]@.

Nosaukums sakrīt ar 'Prelude.concat', tāpēc šo moduli ieteicams importēt
kvalificēti: @import qualified Lab.Concat as C@.
-}
module Lab.Concat
  ( concat
  , concatFold
  , concatElem
  ) where

import Prelude hiding (concat)

-- | Saplacina sarakstu sarakstu (rekursīva definīcija).
--
-- Parametri:
--
--   * @xss@ – sarakstu saraksts.
--
-- Atgriež: visus iekšējo sarakstu elementus vienā sarakstā, secība saglabājas.
--
-- Sarežģītība O(N), kur N ir kopējais elementu skaits: @++@ tiek lietots ar
-- labo asociativitāti, tāpēc katrs elements tiek nokopēts vienu reizi.
-- Funkcija ir slinka un darbojas arī ar bezgalīgiem sarakstiem.
--
-- >>> concat [[1, 2], [3], [], [4, 5]]
-- [1,2,3,4,5]
concat :: [[a]] -> [a]
concat []         = []
concat (xs : xss) = xs ++ concat xss

-- | Ekvivalenta definīcija ar labo locīšanu: @foldr (++) []@.
concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

-- | Ekvivalenta definīcija bez @++@: elementi tiek ņemti pa vienam.
concatElem :: [[a]] -> [a]
concatElem []               = []
concatElem ([] : xss)       = concatElem xss
concatElem ((x : xs) : xss) = x : concatElem (xs : xss)
