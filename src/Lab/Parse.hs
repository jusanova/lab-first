{- |
Modulis  : Lab.Parse
Apraksts : Lietotāja ievades nolasīšana un pārbaude visiem uzdevumiem.

Visas funkcijas ir tīras (bez IO), tāpēc tās var testēt tieši.
Kļūdas tiek atgrieztas kā @Left kļūdas_teksts@, nevis apstādina programmu.
-}
module Lab.Parse
  ( trim
  , parseIntegers
  , parseRational
  , parseNestedList
  ) where

import Data.Char (isDigit, isSpace)
import Data.Ratio ((%))
import Text.Read (readMaybe)

-- | Noņem atstarpes virknes sākumā un beigās.
--
-- >>> trim "  kaķis  "
-- "kaķis"
trim :: String -> String
trim = dropWhileEnd' isSpace . dropWhile isSpace
  where
    dropWhileEnd' p = reverse . dropWhile p . reverse

-- | Pārvērš tekstu veselu skaitļu sarakstā.
--
-- Parametri:
--
--   * @input@ – skaitļi, atdalīti ar komatiem, semikoliem vai atstarpēm;
--     drīkst būt kvadrātiekavās (@"4, -2, 17"@ vai @"[4, -2, 17]"@).
--
-- Atgriež: @Right skaitļi@ (arī tukšu sarakstu) vai @Left kļūdas_teksts@,
-- kurā uzskaitīti visi elementi, kas nav veseli skaitļi.
-- Tips 'Integer' nozīmē, ka nav pārpildes (overflow) riska.
--
-- >>> parseIntegers "5, abc, 7"
-- Left "nederīgi elementi (nav veseli skaitļi): abc"
parseIntegers :: String -> Either String [Integer]
parseIntegers input
  | null bad  = Right [n | Just n <- parsed]
  | otherwise = Left ("nederīgi elementi (nav veseli skaitļi): " ++ unwords bad)
  where
    tokens = words (map separatorToSpace input)
    parsed = map readMaybe tokens :: [Maybe Integer]
    bad    = [t | (t, Nothing) <- zip tokens parsed]
    separatorToSpace c
      | c `elem` ",;[]" = ' '
      | otherwise       = c

-- | Precīzi nolasa decimālskaitli kā racionālu skaitli.
--
-- Parametri:
--
--   * @s@ – teksts, piem. @"-3"@, @"0.25"@ vai @"2,5"@ (komats arī atļauts).
--
-- Atgriež: @Just r@ ar precīzu vērtību vai 'Nothing', ja teksts nav skaitlis.
-- Tā kā @"0.1"@ tiek nolasīts kā precīzi @1/10@, aprēķinos nerodas
-- peldošā punkta noapaļošanas kļūdas.
--
-- >>> parseRational "0.25"
-- Just (1 % 4)
parseRational :: String -> Maybe Rational
parseRational s = case s of
  ('-' : rest) -> negate <$> unsigned rest
  ('+' : rest) -> unsigned rest
  _            -> unsigned s
  where
    unsigned str = case break (`elem` ".,") str of
      (intPart, "")
        | validDigits intPart -> Just (fromInteger (read intPart))
      (intPart, _ : fracPart)
        | validDigits (intPart ++ fracPart) && not (null fracPart) ->
            Just (read (intPart ++ fracPart) % (10 ^ length fracPart))
      _ -> Nothing
    validDigits ds = not (null ds) && all isDigit ds

-- | Nolasa veselu skaitļu sarakstu sarakstu Haskell sintaksē.
--
-- Parametri:
--
--   * @s@ – teksts, piem. @"[[1,2],[3],[],[4,5]]"@.
--
-- Atgriež: @Right xss@ vai @Left kļūdas_teksts@, ja formāts nav korekts
-- (burti, neaizvērtas iekavas, viena līmeņa saraksts).
parseNestedList :: String -> Either String [[Integer]]
parseNestedList s = case readMaybe (trim s) of
  Just xss -> Right xss
  Nothing  -> Left "nekorekts formāts – gaidīts, piem., [[1,2],[3],[],[4,5]]"
