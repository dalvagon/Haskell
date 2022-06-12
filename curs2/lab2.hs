-- Leahu Vlad Iulius 2A6

import Prelude hiding (and, or, not, succ)

-- Ex 1
and :: Bool -> Bool -> Bool
or :: Bool -> Bool -> Bool
not :: Bool -> Bool
nand :: Bool -> Bool -> Bool
nor :: Bool -> Bool -> Bool
imp :: Bool -> Bool -> Bool
dimp :: Bool -> Bool -> Bool

and False _ = False
and _ False = False
and _ _ = True

or False False = False
or _ _ = True

not False = True
not True = False

nand x y = Main.not (Main.and x y)

nor x y = Main.not (Main.or x y)

imp True False = False
imp _ _ = True

dimp x y = Main.and (imp x y) (imp y x)
{-
*Main> and True False
False
*Main> or False False
False
*Main> not True
False
*Main> nand True True
False
*Main> nor True False
False
*Main> imp True False
False
*Main> dimp True False
False
-}


-- Ex 2
hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
                  | mod n a == 0 = True
                  | mod n b == 0 = True
                  | otherwise = hasDivisors n (a + 1) (b - 1)


isPrime :: Integer -> Bool
isPrime n | n == 0 = False
          | n == 1 = False
          | otherwise = not (hasDivisors n 2 (div n 2))

{-
*Main> isPrime 0
False
*Main> isPrime 1
False
*Main> isPrime 2
True
*Main> isPrime 3
True
*Main> isPrime 4
False
*Main> isPrime 5
True
*Main> isPrime 6
False
*Main> isPrime 10
False
*Main> isPrime 121
False
*Main> isPrime 31 
-}

-- Ex 3

cmmdcS :: Integer -> Integer -> Integer
cmmdcS x y | x == 0 = y
           | y == 0 = x
           | x > y = cmmdcS (x - y) y
           | otherwise = cmmdcS x (y - x)

cmmdcI :: Integer -> Integer -> Integer
cmmdcI x y | y == 0 = x
           | otherwise = cmmdcI y (mod x y)

cmmdcBaux :: Integer -> Integer -> Integer -> Integer
cmmdcBaux x 0 r = r * x
cmmdcBaux 0 y r = r * y
cmmdcBaux x y r | and (even x) (even y) = cmmdcBaux (div x 2) (div y 2) 2 * r
                | even x = cmmdcBaux (div x 2) y r
                | even y = cmmdcBaux x (div y 2) r
                | otherwise = cmmdcBaux (abs (x - y)) (min x y) r

cmmdcB :: Integer -> Integer -> Integer
cmmdcB x y = cmmdcBaux x y 1

{-
*Main> cmmdcS 28 21
7
*Main> cmmdcI 28 21
7
*Main> cmmdcB 17 34
17
*Main> cmmdcB 17 20
1
cmmdcB 28 21
7
-}

-- Ex 4
-- Functiile sunt deja tail recursive

-- Ex 5
fibo :: Integer -> Integer
fibo n | n == 0 = 0
       | n == 1 = 1
       | otherwise = fibo (n - 1) + fibo (n - 2)

fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux 0 a _ = a
fiboaux n a b = fiboaux (n - 1) b (a + b)

fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1
{-
Pentru orice n : fibo n = fibo' n
Cazul de baza: n = 0
fibo 0 = 0 (fibo n | n == 0 = 0)
fibo' 0 = fiboaux 0 0 1 = 0 (fiboaux 0 a _ = a)

Cazul inductiv: n > 0
Stiu ca fibo (n - 1) = fibo' (n - 1) => fibo (n - 1) = fiboaux (n - 1) 0 1 (A)
Arat ca: fibo n = fibo' n => fibo n = fiboaux n 0 1
Fie n arbitrar
fibo n = fibo (n - 1) + fibo (n - 2) pentru orice n
fibo n = (din A) (fiboaux (n - 1) 0 1) + fibo (n - 2) pentru orice n
Fie n = n + 1 =>
fibo (n + 1) = (fiboaux n 0 1) + fibo (n - 1) pentru orice n
(din fibo n = fibo (n - 1) + fibo (n - 2)) : fibo n + fibo (n - 1) = (fiboaux n 0 1) + fibo (n - 1) 
fibo n = (fiboaux n 0 1) + fibo (n - 1) - fibo (n - 1) => fibo n = fiboaux n 0 1 => fibo n = fibo' n
-}


-- varianta in O(logn)
fiboaux' :: Integer -> (Integer, Integer)
fiboaux' n | n == 1 = (0, 1)
           | mod n 2 == 0 = (fst (fiboaux' (div n 2)) ^ 2 + snd (fiboaux' (div n 2)) ^ 2, snd (fiboaux' (div n 2)) * (2 * fst (fiboaux' (div n 2)) + snd (fiboaux' (div n 2))))
           | otherwise = (snd (fiboaux' (div n 2)) * (2 * fst (fiboaux' (div n 2)) + snd (fiboaux' (div n 2))), fst (fiboaux' (div n 2)) ^ 2 + snd (fiboaux' (div n 2)) ^ 2 + snd (fiboaux' (div n 2)) * (2 * fst (fiboaux' (div n 2)) + snd (fiboaux' (div n 2))))

fibo'' :: Integer -> Integer
fibo'' n | n == 0 = 0
         | otherwise = snd (fiboaux' n)
{-
| 0  1 | ^ n  | 0 |   =  |  fibo (n - 1)|
| 1  1 |      | 1 |      |  fibo n      |

*Main> fibo'' 10
55
*Main> fibo'' 5
5
*Main> fibo'' 6
8
-}

-- Ex 6
eucE :: Integer -> Integer -> Integer
eucE x y | y == 0 = x
         | otherwise = eucE y (mod x y)

{-
*Main> eucE 21 28
7
*Main> eucE 20 24
4
-}

-- Ex 7
succ :: Integer -> Integer 
succ x = (+) x 1

{-
*Main> succ 0
1
*Main> succ 11
12
*Main> succ (-100)
-99
-}

-- Ex 8
mySum :: Integer -> Integer -> Integer 
mySum x y | x == 0 = y
          | otherwise = succ (mySum (x - 1) y)

myMult :: Integer -> Integer -> Integer -- doesn't work for negative numbers
myMult x 0 = 0
myMult 0 y = 0
myMult x y = mySum x (myMult x (y - 1)) 

myPow :: Integer -> Integer -> Integer 
myPow x 0 = 1
myPow x y = myMult x (myPow x (y - 1))

{-
*Main> myPow 3 3
27
*Main> mySum 10 (-2)
8      
*Main> mySum 10 10  
20     
*Main> myMult 2 3
6
*Main> myMult (-2) 2
*** Exception: stack overflow
*Main> myPow 2 3
8
*Main> myPow 3 3
27
-}

-- Ex 9

myMod :: Integer -> Integer -> Integer 
myMod x y | x >= y = myMod (x - y) y
          | otherwise = x

myDivaux :: Integer -> Integer -> Integer -> Integer 
myDivaux x y r | x >= y = myDivaux (x - y) y (r + 1)
               | otherwise = r
myDiv :: Integer -> Integer -> Integer 
myDiv x y = myDivaux x y 0

{-
*Main> myMod 20 3
2
*Main> myMod 20 10
0
*Main> myMod 10 20
10
*Main> myMod 35 17 
1
*Main> myDiv 22 10
2
*Main> myDiv 100 20
5
*Main> myDiv 13 2  
6
-}