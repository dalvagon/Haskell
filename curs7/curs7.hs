import System.Environment
import System.IO
import Control.Exception
-- import System.Random

-- Monada IO

-- modalitate de a scrie programe
-- de facto imperative intr-un cadru pur functional

-- IO este un tip de date parametrizat

-- valorile de tip "IO a":
--   comenzi/actiuni care, daca le executam pe calculator,
--   produc la sfarsit o valoare de tip "a".
--   ^^^^^^
--   actiunile produc o valoare
--   -nu spun returneaza- (fiindca functiiile returneaza)


-- putStrLn :: String -> IO ()
-- primeste la intrare un sir de caractere si
-- intoarce o actiune care, daca este executata, produce o valoare
--                          ^^^^^^^^^^^^^^^^^^^
--                          afiseze sirul de caractere                      
-- de tip () (citesc: "de tip unit").

-- () :: () (exista o singura valoare, (), de tip ())

-- daca definesc o functie "main" de tip "IO *",
-- compilatorul GHC construieste un executabil care:
-- 1. evalueaza functia main si calculeaza actiunea
-- 2. executa actiunea

-- main :: IO ()
-- main = putStrLn "Hello!"


-- Functia (>>)

-- main = (>>) (putStrLn "Hello!") (putStrLn "World!")

-- main :: IO ()
-- main = putStrLn "Hello!" >>
--        putStrLn "Everyone!" >>
--        putStrLn "World!"

-- getLine :: IO String
-- getLine este o actiune care asteapta un input de la tastatura
-- si la sfarsit produce ca rezultat sirul de caractere introdus
-- la intrarea standard

-- main :: IO ()
-- main = putStrLn "What it your name?" >>
--        (getLine >>= (\x -> putStrLn ("Hello, " ++ x ++ "!")))

-- main :: IO ()
-- main = putStrLn "What it your name?" >>
--        getLine >>=
--        \x -> putStrLn ("Hello, " ++ x ++ "!")

-- zahar sintactic (notatia "do", "do" notation)

-- main :: IO ()
-- main = do putStrLn "What it your name?"
--           x <- getLine
--           putStrLn ("Hello, " ++ x ++ "!")

-- daca nu vreau sa fiu indentation sensitive:
-- main :: IO ()
-- main = do { putStrLn "What it your name?" ;
--           x <- getLine ;
--         putStrLn ("Hello, " ++ x ++ "!") }

-- nu merge daca gresesc indentarea
-- main :: IO ()
-- main = do putStrLn "What it your name?"
--         x <- getLine
--           putStrLn ("Hello, " ++ x ++ "!")

-- Vreau sa nu confund x <- getLine, care este o notatie,
-- cu let x = getLine.
-- main :: IO ()
-- main = do putStrLn "What it your name?"
--           let x = getLine in
--             x >>=
--             (\y -> putStrLn ("Hello, " ++ y ++ "!"))
          
-- cand scriu let x = getLine, x devine un sinonim pentru getLine

-- cum obtinem comportamente repetitive

-- main :: IO ()
-- main = do putStrLn "Hello! What is your name?"
--           x <- getLine
--           if x == "" then
--             -- actiunea pe care o intoarce main daca x este sirul vid
--             return ()
--           else
--             -- actiunea pe care o intoarce main daca x nu este sirul vid
--             do putStrLn ("Nice to meet you, " ++ x ++ "!")
--                main
               
-- return x este o actiunea care nu efectueaza nimic
-- si la sfarsit produce valoarea x

-- Atentie! nu este acelasi lucru cu return-ul din C!!!
-- main :: IO ()
-- main = do return ()
--           putStrLn "Hello! What is your name?"

-- este sintactic sugar pentru 
-- main :: IO ()
-- main = return () >> putStrLn "Hello! What is your name?"

-- (G, +, ...) este un grup daca ...

-- (M, >>=, return) este o monada
--  daca respecta anumite legi.


-- de facto, functiile care intorc actiuni IO sunt programe imperative

-- cand scriu un program mare:
--    prefer cat mai multe functii sa fie functii pure
--    si cat mai putine in monada IO

-- O intrebare frecventa:
-- am functia getLine :: IO String
-- cum scriu getLine :: String?
-- Raspuns: nu se poate!!

-- cum "punem mana" pe argumentele din linia de comanda

-- main :: IO ()
-- main = do xs <- getArgs
--           print xs


-- main :: IO ()
-- main = do xs <- getArgs
--           name <- getProgName
--           putStrLn name
--           print xs

-- main :: IO ()
-- main = do xs <- getArgs
--           name <- getProgName
--           putStrLn name
--           print xs

-- proceseaza :: [String] -> IO ()
-- proceseaza [] = return ()
-- proceseaza (x:xs) = do putStrLn x
--                        proceseaza xs

-- main :: IO ()
-- main = do xs <- getArgs
--           proceseaza xs

-- cum interactioneaza evaluarea lenesa cu monada IO ?


-- facem un program care citeste un fisier sursa
-- Haskell si scriem la iesirea std fara comentarii (linii care incep cu --)

-- main :: IO ()
-- main = do (filename:_) <- getArgs
--           handle <- openFile filename ReadMode
--           continut <- hGetContents handle
--           putStrLn continut

-- sunt doua optiuni:
--    sau scriu o functie pura care proceseaza tot string-ul "continut"
--    sau scriu o functie IO care proceseaza fisierul linie cu linie

-- isComment :: String -> Bool
-- isComment ('-':'-':_) = True
-- isComment _ = False -- aici e partea interesanta

-- proceseaza :: Handle -> IO ()
-- proceseaza handle = do b <- hIsEOF handle
--                        if b then
--                          return ()
--                        else do
--                          line <- hGetLine handle
--                          if isComment line then
--                            return ()
--                          else
--                            putStrLn line
--                          proceseaza handle

-- trateaza :: IOException -> IO ()
-- trateaza e = putStrLn "Ooops!"

-- main :: IO ()
-- main = do (filename:_) <- getArgs
--           (do handle <- openFile filename ReadMode
--               proceseaza handle) `catch` trateaza

-- alte exceptii

tratare :: ArithException -> IO ()
tratare e = putStrLn "A aparut o problema!!!"

main :: IO ()
main = do putStrLn (show (div 10 0)) `catch` tratare



-- Atentie!
-- daca lucrati cu o instalare "simpla" a ghc: nu aveti acces la
-- System.Random

-- Haskell Platform: ok!

-- Ubuntu/MacOSX: apt-get/homebrew ca sa instalati stack

-- stack este un Haskell Package manager

-- 1. stack install random
-- 2. stack ghci in loc de "ghci"

-- nu putem implementa
-- random :: Int
-- random = 7 -- ??

-- stare: 372183721 (variabila globala)
-- random() --> 1. intoarce starea
--              2. stare = stare * 432178432 + 423874

-- f :: (Int, Int)
-- f = let stare1 = mkStdGen 78787 in
--     let (n1, stare2) = random stare1 in
--     let (n2, stare3) = random stare2 in
--     (n1, n2)

-- exercitiu pentru laborator:

-- scriu un main in care creez un generator cu un seed
-- dat de ora curenta
-- si scriu 10 numere random pornind de la seedul respectiv

{-
altfel spus: de tradus in Haskell programul de mai jos
srand(time(0));
printf("%d", rand());
printf("%d", rand());
printf("%d", rand());
printf("%d", rand());
printf("%d", rand());
printf("%d", rand());
printf("%d", rand());
printf("%d", rand());
-}