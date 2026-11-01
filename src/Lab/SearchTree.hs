{- |
Modulis  : Lab.SearchTree
Uzdevums : 7. uzdevums
Apraksts : Binārais koks, binārā meklēšanas koka (BMK) pārbaude un
           ievietošana.

== BMK definīcija (atbilst 'insertTree')

Katrā virsotnē @Branch x l r@ visas @l@ vērtības ir @<= x@, bet visas @r@
vērtības ir @> x@ – vienādas vērtības 'insertTree' liek pa kreisi.

== Par tipu

Uzdevumā dotais tips @Tree a -> Bool@ nav realizējams: elementu
salīdzināšanai vajadzīga klase 'Ord', tāpēc tips ir @Ord a => Tree a -> Bool@.

Īpašība “ievietošana saglabā BMK” un eksperimenti ar kļūdainām 'insertTree'
versijām atrodas testos (@test/Lab/SearchTreeSpec.hs@).
-}
module Lab.SearchTree
  ( Tree (..)
  , isSearchTree
  , insertTree
  , fromList
  , toList
  , size
  ) where

-- | Binārais koks: virsotne ar vērtību un diviem apakškokiem, vai lapa.
data Tree a = Branch a (Tree a) (Tree a) | Leaf
  deriving (Eq, Show)

-- | Pārbauda, vai koks ir binārais meklēšanas koks.
--
-- Parametri:
--
--   * @t@ – pārbaudāmais koks.
--
-- Atgriež: 'True', ja katra vērtība atbilst /visu/ senču robežām, ne tikai
-- tiešajiem bērniem.
--
-- Algoritms: koks tiek apstaigāts no saknes, līdzi nesot pieļaujamo
-- intervālu @(lo, hi]@. Ejot pa kreisi, augšējā robeža kļūst @x@; ejot pa
-- labi, apakšējā robeža kļūst @x@. 'Nothing' nozīmē “robežas nav”.
-- Sarežģītība O(n).
--
-- >>> isSearchTree (Branch 5 (Branch 3 Leaf (Branch 6 Leaf Leaf)) Leaf)
-- False
isSearchTree :: Ord a => Tree a -> Bool
isSearchTree = go Nothing Nothing
  where
    go _ _ Leaf = True
    go lo hi (Branch x l r) =
      aboveLow && belowHigh && go lo (Just x) l && go (Just x) hi r
      where
        aboveLow  = maybe True (< x) lo    -- x > lo (stingri)
        belowHigh = maybe True (x <=) hi   -- x <= hi

-- | Ievieto vērtību binārajā meklēšanas kokā (uzdevumā dotā realizācija).
--
-- Parametri:
--
--   * @e@ – ievietojamā vērtība;
--   * @t@ – koks.
--
-- Atgriež: jaunu koku ar @e@. Oriģinālā otrais nosacījums ir @e > x@; šeit
-- tas ir @otherwise@ (vienīgais atlikušais gadījums), lai kompilators
-- ar @-Wall@ nebrīdinātu par nepilnīgiem gadījumiem.
insertTree :: Ord a => a -> Tree a -> Tree a
insertTree e Leaf = Branch e Leaf Leaf
insertTree e (Branch x li re)
  | e <= x    = Branch x (insertTree e li) re
  | otherwise = Branch x li (insertTree e re)

-- | Izveido koku, pa vienam ievietojot saraksta elementus ar 'insertTree'.
fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insertTree) Leaf

-- | Koka elementi “inorder” secībā (kreisais, sakne, labais).
-- BMK gadījumā rezultāts ir sakārtots.
toList :: Tree a -> [a]
toList t = go t []
  where
    go Leaf acc           = acc
    go (Branch x l r) acc = go l (x : go r acc)

-- | Virsotņu skaits kokā.
size :: Tree a -> Int
size = length . toList
