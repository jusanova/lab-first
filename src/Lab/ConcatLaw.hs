{- |
Modulis  : Lab.ConcatLaw
Uzdevums : 8. uzdevums
Apraksts : Likuma  @map f . concat = concat . map (map f)@  pierādījums ar
           matemātisko indukciju. Funkcijas 'lhs' un 'rhs' ir likuma abas
           puses; testi tās salīdzina ar nejaušiem datiem.

== Definīcijas

@
map f []        = []                     -- (map.1)
map f (x : xs)  = f x : map f xs         -- (map.2)
concat []       = []                     -- (concat.1)
concat (xs:xss) = xs ++ concat xss       -- (concat.2)
[] ++ ys        = ys                     -- (++.1)
(x : xs) ++ ys  = x : (xs ++ ys)         -- (++.2)
@

Pēc ekstensionalitātes pietiek pierādīt, ka katram galīgam @xss@
izpildās @map f (concat xss) = concat (map (map f) xss)@.

== Lemma: @map f (xs ++ ys) = map f xs ++ map f ys@ (indukcija pēc @xs@)

/Bāze/:

@
map f ([] ++ ys) = map f ys = [] ++ map f ys = map f [] ++ map f ys
@

/Solis/:

@
map f ((x : xs) ++ ys) = map f (x : (xs ++ ys))         -- (++.2)
                       = f x : map f (xs ++ ys)         -- (map.2)
                       = f x : (map f xs ++ map f ys)   -- ind. pieņēmums
                       = (f x : map f xs) ++ map f ys   -- (++.2)
                       = map f (x : xs) ++ map f ys     -- (map.2)
@

== Teorēma (indukcija pēc @xss@)

/Bāze/:

@
map f (concat []) = map f [] = [] = concat [] = concat (map (map f) [])
@

/Solis/:

@
map f (concat (xs : xss))
  = map f (xs ++ concat xss)                    -- (concat.2)
  = map f xs ++ map f (concat xss)              -- lemma
  = map f xs ++ concat (map (map f) xss)        -- ind. pieņēmums
  = concat (map f xs : map (map f) xss)         -- (concat.2)
  = concat (map (map f) (xs : xss))             -- (map.2)
@

Tātad @map f . concat = concat . map (map f)@.  ∎
-}
module Lab.ConcatLaw
  ( lhs
  , rhs
  , proofSteps
  ) where

-- | Likuma kreisā puse: vispirms saplacina, tad pielieto @f@.
lhs :: (a -> b) -> [[a]] -> [b]
lhs f = map f . concat

-- | Likuma labā puse: vispirms pielieto @f@ iekšējos sarakstos, tad saplacina.
rhs :: (a -> b) -> [[a]] -> [b]
rhs f = concat . map (map f)

-- | Induktīvā soļa starpizteiksmes konkrētam piemēram.
--
-- Parametri:
--
--   * @f@ – funkcija;
--   * @xs@ – pirmais iekšējais saraksts;
--   * @xss@ – pārējie iekšējie saraksti.
--
-- Atgriež: pārus (pamatojums, vērtība). Ja pierādījums ir pareizs, visas
-- vērtības ir vienādas.
proofSteps :: (a -> b) -> [a] -> [[a]] -> [(String, [b])]
proofSteps f xs xss =
  [ ("map f (concat (xs : xss))",                       map f (concat (xs : xss)))
  , ("map f (xs ++ concat xss)              (concat.2)", map f (xs ++ concat xss))
  , ("map f xs ++ map f (concat xss)        (lemma)",    map f xs ++ map f (concat xss))
  , ("map f xs ++ concat (map (map f) xss)  (ind.)",     map f xs ++ concat (map (map f) xss))
  , ("concat (map f xs : map (map f) xss)   (concat.2)", concat (map f xs : map (map f) xss))
  , ("concat (map (map f) (xs : xss))       (map.2)",    concat (map (map f) (xs : xss)))
  ]
