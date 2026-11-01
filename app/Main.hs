
{- |
Modulis  : Main
Apraksts : Interaktīva programma visiem laboratorijas darba uzdevumiem.

Palaišana:

@
cabal run lab-first          -- izvēlne
cabal run lab-first -- 3     -- uzreiz 3. uzdevums
@

Šeit ir tikai ievade un izvade. Visa loģika ir bibliotēkā (@src/Lab@),
bet testi – atsevišķā testu komplektā (@test/@).
-}
module Main (main) where

import qualified Lab.Concat as C
import Lab.Capitalize (capitalizeSentence)
import Lab.ConcatLaw (lhs, rhs)
import Lab.MaxNumber (describeMax)
import Lab.Parse (parseIntegers, parseNestedList, parseRational, trim)
import Lab.Quadratic (describeSol, numbOfSol, showRational)
import Lab.RemoveAll (removeAllOrd)
import Lab.ReverseWord (describeReverse)
import Lab.SearchTree (fromList, isSearchTree, toList)

import Control.Monad ((<=<))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, hSetEncoding, stderr, stdin, stdout)
import System.IO.Error (catchIOError)

-- | Viens uzdevums interaktīvajā programmā.
data Task = Task
  { taskTitle  :: String  -- ^ nosaukums izvēlnē
  , taskAction :: Action  -- ^ kāda ievade vajadzīga un kā to apstrādāt
  }

-- | Uzdevuma ievades forma. Tips garantē, ka apstrādes funkcija saņem tieši
-- tik rindu, cik tiek prasīts.
data Action
  = OneLine String (String -> String)
    -- ^ viena rinda: uzaicinājums un apstrāde
  | TwoLines String String (String -> String -> String)
    -- ^ divas rindas: divi uzaicinājumi un apstrāde

-- | Visi uzdevumi; numurs sarakstā sakrīt ar uzdevuma numuru.
tasks :: [Task]
tasks =
  [ Task "Maksimālais skaitlis sarakstā" $
      OneLine "Skaitļi (piem. 5, 10, 11, 15, 3, 8)"
        (either ("Kļūda: " ++) describeMax . (nonEmpty <=< parseIntegers))
  , Task "Vārda apgriešana" $
      OneLine "Vārds" describeReverse
  , Task "Katra vārda pirmais burts lielais" $
      OneLine "Teikums"
        (\s -> if null (words s) then "Kļūda: teikums ir tukšs." else capitalizeSentence s)
  , Task "Kvadrātvienādojuma atrisinājumu skaits" $
      OneLine "Koeficienti h i j (piem. 1 -5 6)" quadratic
  , Task "Sarakstu saraksta saplacināšana (concat)" $
      OneLine "Sarakstu saraksts (piem. [[1,2],[3],[],[4,5]])"
        (either ("Kļūda: " ++) (\xss -> "Rezultāts: " ++ show (C.concat xss)) . parseNestedList)
  , Task "Otrā saraksta elementu izņemšana no pirmā" $
      TwoLines "Pirmais saraksts (piem. 1, 2, 3, 2, 4)" "Izņemamie elementi (piem. 2, 4)" removeTask
  , Task "Binārais meklēšanas koks" $
      OneLine "Ievietojamās vērtības (piem. 5, 2, 8, 2, 9)"
        (either ("Kļūda: " ++) describeTree . parseIntegers)
  , Task "Likums map f . concat = concat . map (map f), f = (*10)" $
      OneLine "Sarakstu saraksts (piem. [[1,2],[3],[],[4,5]])"
        (either ("Kļūda: " ++) describeLaw . parseNestedList)
  ]
  where
    nonEmpty [] = Left "ievade ir tukša – ievadiet vismaz vienu skaitli."
    nonEmpty xs = Right xs

    removeTask a b = case (parseIntegers a, parseIntegers b) of
      (Left e, _)          -> "Kļūda pirmajā sarakstā: " ++ e
      (_, Left e)          -> "Kļūda otrajā sarakstā: " ++ e
      (Right xs, Right ys) -> "Rezultāts: " ++ show (removeAllOrd xs ys)

    describeTree xs =
      let t = fromList xs
      in "Koks: " ++ show t ++ "\nInorder: " ++ show (toList t)
           ++ "\nIr BMK: " ++ show (isSearchTree t)

    describeLaw xss =
      "map f . concat       = " ++ show (lhs (* 10) xss)
        ++ "\nconcat . map (map f) = " ++ show (rhs (* 10) xss)
        ++ "\nAbas puses vienādas: " ++ show (lhs (* 10) xss == rhs (* 10) xss)

-- | 4. uzdevuma ievades apstrāde: trīs precīzi nolasīti koeficienti.
quadratic :: String -> String
quadratic s = case map parseRational (words s) of
  [Just h, Just i, Just j] ->
    "(" ++ showRational h ++ ")x² + (" ++ showRational i ++ ")x + ("
      ++ showRational j ++ ") = 0  →  " ++ describeSol (numbOfSol h i j)
  ms | length ms /= 3 -> "Kļūda: jāievada tieši 3 skaitļi, ievadīti " ++ show (length ms) ++ "."
     | otherwise      -> "Kļūda: visiem koeficientiem jābūt skaitļiem."

-- | Programmas ieeja: ar argumentu palaiž konkrētu uzdevumu, bez tā – izvēlni.
main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  args <- getArgs
  case args of
    []    -> menu
    [arg] | Just n <- taskNumber arg -> runTask n
    _     -> do
      hPutStrLn stderr ("Lietojums: lab-first [1-" ++ show (length tasks) ++ "]")
      exitFailure

-- | Pārvērš argumentu derīgā uzdevuma numurā.
taskNumber :: String -> Maybe Int
taskNumber s = case reads s of
  [(n, "")] | n >= 1 && n <= length tasks -> Just n
  _                                       -> Nothing

-- | Galvenā izvēlne; atkārtojas, līdz ievadīts @q@ vai beidzas ievade.
menu :: IO ()
menu = do
  putStrLn "\n=== Laboratorijas darbs Nr. 1 ==="
  mapM_ (\(n, t) -> putStrLn (show n ++ ". " ++ taskTitle t)) (zip [1 :: Int ..] tasks)
  answer <- ask "Izvēlieties uzdevumu (q – beigt)"
  case answer of
    Nothing -> putStrLn "Programma beidz darbu."
    Just s  -> case taskNumber s of
      Just n  -> runTask n >> menu
      Nothing -> putStrLn "Kļūda: nav tāda uzdevuma." >> menu

-- | Cikliski izpilda vienu uzdevumu, līdz ievadīts @q@ vai beidzas ievade.
runTask :: Int -> IO ()
runTask n = do
  let task = tasks !! (n - 1)
  putStrLn ("\n--- " ++ show n ++ ". " ++ taskTitle task ++ " ---")
  loop (taskAction task)
  where
    loop action = do
      result <- case action of
        OneLine p f -> fmap f <$> ask p
        TwoLines p1 p2 f -> do
          a <- ask p1
          case a of
            Nothing -> return Nothing
            Just x  -> fmap (f x) <$> ask p2
      case result of
        Nothing  -> putStrLn "Atpakaļ uz izvēlni."
        Just out -> putStrLn out >> loop action

-- | Parāda uzaicinājumu un nolasa rindu. Atgriež 'Nothing', ja ievadīts @q@
-- vai ievade beigusies (EOF), tāpēc programma nekad neapstājas ar izņēmumu.
ask :: String -> IO (Maybe String)
ask prompt = do
  putStr (prompt ++ ": ")
  hFlush stdout
  line <- catchIOError (Just <$> getLine) (\_ -> return Nothing)
  return $ case line of
    Just l | trim l /= "q" -> Just l
    _                      -> Nothing
