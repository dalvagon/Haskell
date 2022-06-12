import Data.Char
import System.IO
import System.Environment
import Control.Exception
import Data.Time.Clock.System (getSystemTime, SystemTime (systemSeconds, systemNanoseconds))

-- Leahu Vlad A6

-- EX 1
-- singura valoare de tip () este ()


-- EX 2
main2 :: IO ()
main2 = putStrLn "Who are you?" >>
       putStrLn "Answer!" >>
       putStrLn "OK!"

{-
:i (>>)
type Monad :: (* -> *) -> Constraint
class Applicative m => Monad m where
  ...
  (>>) :: m a -> m b -> m b
  ...
        -- Defined in `GHC.Base'
infixl 1 >> <- asociaza la dreapta
-}


-- EX 3
main3 :: IO ()
main3 = putStrLn "Prenume?" >>
        (getLine >>=
            (\x -> putStrLn "Nume?" >>
                (getLine >>=
                    (\y -> putStrLn $"Hello, " ++ x ++ " " ++ y ++ "!"))))
{-
*Main> main3
Prenume?
Vlad
Nume?
Leahu
Hello, Vlad Leahu!
-}

-- EX 4
main4 :: IO ()
main4 = do putStrLn "Prenume?"
           prenume <- getLine
           putStrLn "Nume"
           nume <- getLine
           putStrLn $ "Hello, " ++ nume ++ " " ++ prenume ++ "!"

-- EX 5
main5' :: IO ()
main5' = do putStrLn "What is your name?"
            name <- getLine
            putStrLn ("Hello, " ++ name ++ "!")
            main5'

main5 :: IO ()
main5 = putStrLn "What is your name?" >>
        (getLine >>= (\name -> putStrLn $ "Hello, " ++ name ++ "!")) >>
        main5
{-
*Main> main5
What is your name?
Lehau Vlad
Hello, Lehau Vlad!
What is your name?
Leahu Vlad
Hello, Leahu Vlad!
What is your name?
a
Hello, a!
What is your name?
b
Hello, b!
What is your name?
c
Hello, c!
What is your name?
-}

-- EX 6
main6 = putStrLn "Prenume?" >>
        (getLine >>=
            (\x -> putStrLn "Nume?" >>
                (getLine >>=
                    (\y -> putStrLn $"Hello, " ++ x ++ " " ++ y ++ "!")))) >>
                        main6
{-
*Main> main6
Prenume?
Vlad
Nume?
Leahu
Hello, Vlad Leahu!
Prenume?
A
Nume?
B
Hello, A B!
Prenume?


main :: IO ()
main = do
putStrLn "What is your name?"
name <- return "Victor"
putStrLn ("Hello, " ++ name ++ "!")

-- Afiseaza Hello, Victor!, deoarece return "Victor" intoarce o actiune care
produce un String


main :: IO ()
main = do
putStrLn "What is your name?"
return ()
name <- getLine
putStrLn ("Hello, " ++ name ++ "!")

-- return () intoarce o actiune care nu produce nimic - se afiseaza 
Hello, numele introdus!
-}

-- EX 7
main7 :: IO ()
main7 = do putStrLn "Prenume?"
           prenume <- getLine
           putStrLn "Nume"
           nume <- getLine
           if nume == "" || prenume == ""  then
               return ()
           else do
               putStrLn $ "Hello, " ++ nume ++ " " ++ prenume ++ "!"
               main7
{-
*Main> main7
Prenume?
A
Nume
B
Hello, B A!
Prenume?
D
Nume
C
Hello, C D!
Prenume?

Nume
A
-}

upper :: String -> String
upper = map toUpper

-- EX 8
main8 :: IO ()
main8 = do putStrLn "Sir:"
           sir <- getLine
           putStrLn $ upper sir
           main8
{-
*Main> main8
Sir:
al
AL
Sir:
ALLA
ALLA
Sir:
alkjfhjkerfh
ALKJFHJKERFH
Sir:
***Interrupted.
-}

-- EX 9
{-
*Main> :m + System.IO
*Main System.IO> :i openFile
openFile :: FilePath -> IOMode -> IO Handle

*Main System.IO> :i hGetContents 
hGetContents :: Handle -> IO String

*Main System.IO> :i hGetLine 
hGetLine :: Handle -> IO String  

*Main System.IO> :i hClose 
hClose :: Handle -> IO () 

Main System.IO> :m + System.Environment
*Main System.IO System.Environment> :i getArgs 
getArgs :: IO [String]

*Main System.IO System.Environment> :i getProgName 
getProgName :: IO String

*Main System.IO System.Environment> :i hPutStr
hPutStr :: Handle -> String -> IO ()
-}

-- EX 10
main10 :: IO ()
main10 = do handle <- openFile "./exemplu.txt" ReadMode
            content <- hGetContents handle
            putStrLn content

{-
*Main> main10
First, let's take a look at the reverseWords function. It's just a normal function that takes a string like "hey there man" and then calls words with it to produce a list of words like ["hey","there","man"]. Then we map reverse on the list, gett ...
-}

-- EX 11
-- main :: IO ()
-- main = do (filename:_) <- getArgs
--           handle <- openFile filename ReadMode
--           content <- hGetContents handle
--           putStrLn content
{-
PS C:\Users\Vlad\OneDrive\Desktop\pf\curs7> ghc lab7.hs                  
[1 of 1] Compiling Main             ( lab7.hs, lab7.o )
Linking lab7.exe ...
PS C:\Users\Vlad\OneDrive\Desktop\pf\curs7> ./lab7 .\exemplu.txt
First, let's take a look at the reverseWords function. It's just a normal function that takes a string like "hey there man" and then calls words with it to produce a list of ...
-}

-- EX 12
process :: Handle -> IO ()
process handle = do content <- hGetContents handle
                    putStrLn content

printArgs :: [String] -> IO ()
printArgs [] = return ()
printArgs (x : xs) = do putStr x
                        printArgs xs

treat :: IOException -> IO ()
treat e = do xs <- getArgs
             progname <- getProgName
             putStr progname
             printArgs xs

-- main :: IO ()
-- main = do (filename:_) <- getArgs
--           (do handle <- openFile filename ReadMode
--               process handle) `catch` treat
{-
PS C:\Users\Vlad\OneDrive\Desktop\pf\curs7> ./lab7 .\exemplu.tx 
lab7.exe.\exemplu.tx
PS C:\Users\Vlad\OneDrive\Desktop\pf\curs7> ./lab7 .\asjfh
lab7.exe.\asjfh
-}

-- EX 13

processUpper :: Handle -> IO ()
processUpper handle = do content <- hGetContents handle
                         putStrLn $ upper content

-- main :: IO ()
-- main = do (filename:_) <- getArgs
--           (do handle <- openFile filename ReadMode
--               processUpper handle) `catch` treat
{-
runhaskell lab7.hs .\exemplu.txt -- runhaskell functioneaza mai repede
FIRST, LET'S TAKE A LOOK AT THE REVERSEWORDS FUNCTION. IT'S JUST A NORMAL FUNCTION THAT TAKES A STRING LIKE "HEY THERE MAN" AND THEN CALLS WORDS WITH IT TO PRODUCE A LIST OF WORDS LIKE ["HEY","THERE","MAN"]. THEN WE MAP REVERSE ON THE LIST, GETTING ["YEH","EREHT","NAM"] AND THEN WE PUT THAT BACK INTO ONE STRING BY USING UNWORDS AND THE FINAL RESULT IS "YEH EREHT NAM". SEE HOW WE USED FUNCTION COMPOSITION HERE. WITHOUT FUNCTION COMPOSITION, WE'D HAVE TO WRITE SOMETHING LIKE REVERSEWORDS ST = UNWORDS (MAP REVERSE (WORDS ST)).
-}

-- Ex 14
firstHalf' :: [a] -> Int -> [a]
firstHalf' [] _ = []
firstHalf' (x : xs) 0 = [x]
firstHalf' (x : xs) len = x : firstHalf' xs (len - 1)

firstHalf :: [a] -> [a]
firstHalf [x] = []
firstHalf l = firstHalf' l (length l `div` 2 - 1)


secondHalf' :: [a] -> Int -> [a]
secondHalf' [] _ = []
secondHalf' l len
        | len == length l - 1 = [(!!) l len]
        | otherwise = (!!) l len : secondHalf' l (len + 1)

secondHalf :: [a] -> [a]
secondHalf l = secondHalf' l (length l `div` 2)

helper :: [Int] -> IO ()
helper [x] = do putStrLn $ "Atunci numarul este " ++ show x
helper (y : ys) = do putStrLn $ "Numarul de ghicit este >= " ++ show ((!!) (y : ys) ((length (y : ys)) `div` 2))
                     raspuns <- getLine
                     if raspuns == "Nu" then
                        helper (firstHalf (y : ys))
                     else
                        if raspuns == "Da" then
                            helper (secondHalf (y : ys))
                        else
                            do putStrLn "Nu am inteles!"
                               helper (y : ys)

-- main :: IO ()
-- main = do helper [0..100]

{-
-- ghicire 17
PS C:\Users\Vlad\OneDrive\Desktop\pf\curs7> runhaskell lab7.hs
Numarul de ghicit este >= 50
df
Nu am inteles!
Numarul de ghicit este >= 50
Nu
Numarul de ghicit este >= 25
Nu
Numarul de ghicit este >= 12
Da
Numarul de ghicit este >= 18
Nu
Numarul de ghicit este >= 15
Da
Numarul de ghicit este >= 16
Da
Numarul de ghicit este >= 17
Da
Atunci numarul este 17
-}

-- EX 15
{-
Evaluarea lenesa este esentiala in cazul stream-urilor infinite de date, dar evaluarea lenesa pentru executia actiunilor IO
este de bine de ocolit deoarece nu se poate sti cu siguranta can un handle este inchis, sau cand am terminat de citit un fisier.  
-}


-- EX 16
{-
main :: IO ()
main = do handle <- openFile "exemplu.txt" ReadMode   
          content <- hGetContents handle
          hClose handle
          putStrLn content

PS C:\Users\Vlad\OneDrive\Desktop\pf\curs7> runhaskell lab7.hs
lab7.hs: exemplu.txt: hGetContents: illegal operation (delayed read on closed handle)

main :: IO ()
main = do handle <- openFile "exemplu.txt" ReadMode   
          content <- hGetContents handle
          putStrLn content
          hClose handle

PS C:\Users\Vlad\OneDrive\Desktop\pf\curs7> runhaskell lab7.hs
a


Schimband ordinea ultimelor 2 linii, se obtin outputuri diferite, deoarece hGetContents citeste din fisier 
doar cand se apleaza putStrLn content. Dar in primul caz, handle-ul este deja inchis, deci se va intoarce o eroare
-}

--EX 17
printNumber :: Int -> IO ()
printNumber 0 = do return ()
printNumber n = do time <- getSystemTime
                   print (systemNanoseconds time * 4 + 3 - 12749  `div` 2)
                   printNumber (n - 1)

main :: IO ()
main = do (n : _) <- getArgs
          printNumber (read n :: Int)


