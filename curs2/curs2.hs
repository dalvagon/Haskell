fibo  :: Integer -> Integer

fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

fibo' :: Integer -> Integer
-- "guard"
fibo' x | x == 0 || x == 1 = 1
fibo' n                    = fibo' (n - 1) + fibo' (n - 2)

fibo'' :: Integer -> Integer
-- "guard"
fibo'' x
        | x == 0 = 1
        | x == 1 = 1
fibo'' n         = fibo'' (n - 1) + fibo'' (n - 2)


fibo''' :: Integer -> Integer
-- "guard"
fibo''' x
        | x == 0 = 1
        | x == 1 = 1
        | otherwise = fibo''' (x - 1) + fibo' (x - 2)

-- de preferat sa definim funvtii evitand expresiile conditionale
-- preferam sa folosim pattern-uri sau garzi



cmmdc :: Integer -> Integer -> Integer
cmmdc x 0 = x
cmmdc 0 y = y
cmmdc x y   | (>) x y =  cmmdc (x - y) y
            | otherwise = cmmdc x ((-) y x)

sumN :: Integer -> Integer
sumN 0 = 0
sumN n = n + sumN (n - 1)


-- tranformare sumN intr-o functie cu acumulator
sumN' :: Integer -> Integer  -> Integer
sumN' 0 a = a
sumN' n a = sumN' (n - 1) (a + n)
--          ^^^^
--          tail call

-- Orice functie tail-recursive poate fi transformata de_facto intr-o bucla
-- (care este deosebit de eficienta)

-- Avantajul transparentei referentiale
-- Calculeaza cele doua functii acelasi rezultat? Pot demonstra prin rationament ecuational

-- sumN :: Integer -> Integer 
-- sumN 0 = 0 
-- sumN n = n + sumN (n - 1)
-- sumN' :: Integer -> Integer  -> Integer 
-- sumN' 0 a = a  
-- sumN' n a = sumN' (n - 1) (a + n)

{- 
    pentru orice n : sumN' n 0 = sumn n.
    pentru orice n : sumN' n 0 = a + sumn n.
-}

-- Tipul de date "pair" 

myFst :: (Integer, Integer) -> Integer
myFst x  = fst x 

myFst' :: (Integer, Integer) -> Integer
myFst' (x, y) = x -- pattern pentru perechi 

myFst'' :: (Integer, Integer) -> Integer
myFst'' (x, _) = x -- pattern pentru perechi 

-- ATENTIE! myFst, myFst', myFst'' au un singur argument ( argumentul este o pereche)

mySum3 :: (Integer, Integer, Integer) -> Integer 
mySum3 (x, y, z) = x + y + z

-- Tipul de date lista

sumList :: [Integer] -> Integer 
sumList [] = 0
sumList (x:xs) = x + sumList xs

wishful :: Integer -> Integer 
wishful n = wishful n

-- In Haskell, nu exista nicio diferenta intre o valoare de tip 
-- Integere si o functie care nu are niciun argument si intoarce un Integer

a1 :: Integer 
a1 = a1

sumList' :: [Integer] -> Integer
sumList' (x:xs) = x + sumList' xs 
sumList' [] = 0
