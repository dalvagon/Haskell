{-
Prelude> (+) 2 3
5

Prelude> (*) 2 3
6

Prelude> (*) ((+) 2 3) 5
25

Prelude> (/) 3 0
Infinity

Prelude> (&&) True False
False

Prelude> (||) ((<=) 2 3) False
True

Prelude> (++) "Aaa" "Aba"
"AaaAba"

Prelude> :t True
True :: Bool

Prelude> :t True && False
True && False :: Bool


Prelude> :t True && (2 <= 4)
True && (2 <= 4) :: Bool

Prelude> :t not
not :: Bool -> Bool

Prelude> :t not True
not True :: Bool

Prelude> :t "aaa"
"aaa" :: [Char]

Prelude> :t 2
2 :: Num p => p

Prelude> :t 2 + 3
2 + 3 :: Num a => a

Prelude> :t (+)  
(+) :: Num a => a -> a -> a

Prelude> succ 2
3
Prelude> :t succ
succ :: Enum a => a -> a

Prelude> max 2 3
3
Prelude> :t max
max :: Ord a => a -> a -> a

-}

-- Prelude> id x = x
-- Prelude> id 2
-- 2

id x = x

sumThree x y z = x + y + z

{-
Main> Main.id (True || (3 > ((+) 2 5)))
True

*Main> sumThree 2 3 4
9

*Main> :t sumThree
sumThree :: Num a => a -> a -> a -> a

*Main> sumThree 3.2 2 4
9.2
-}

sumThreeInt :: Int -> Int -> Int -> Int
sumThreeInt x y z = x + y + x

myMax :: Int -> Int -> Int
myMax x y = if x <= y then y else x

{-
Main> :t sumThreeInt 2 3 4
sumThreeInt 2 3 4 :: Int

*Main> :t sumThreeInt 2 3 4.5
<interactive>:1:17: error:
    * No instance for (Fractional Int) arising from the literal `4.5'
    * In the third argument of `sumThreeInt', namely `4.5'
      In the expression: sumThreeInt 2 3 4.5

*Main> :t myMax 
myMax :: Int -> Int -> Int

*Main> myMax 3 100
100
-}


-- [Ex.  3.10]

myMaxThree :: Int -> Int -> Int -> Int
myMaxThree x y z =
       if x <= y then
              if y <= z then z
              else y
       else
              if x <= z then z
              else x

{-
Main> myMaxThree  100 200 300
300
*Main> myMaxThree  300 200 100
300
-}

mySum :: Int -> Int
mySum x = if x <= 0 then 0 else x + mySum (x - 1)

{-
Main> mySum 100
5050
-}


-- [Ex. 3.12]

myFib :: Int -> Int
myFib x = if x <= 1 then x else myFib (x - 1) + myFib (x - 2)

{-
Main> myFib 0
0
*Main> myFib 1
1
*Main> myFib 2
1
*Main> myFib 3
2
*Main> myFib 4
3
*Main> myFib 5
5
*Main> myFib 6
8
-}

-- [Ex. 3.13]
cmmdc :: Int -> Int -> Int
cmmdc x y
  | (==) x y = x
  | x > y = cmmdc (x - y) y
  | otherwise = cmmdc x (y -x)

{-
Using guards(hint from editor):

cmmdc :: Int -> Int -> Int
cmmdc x y
  | ((==) x y) = x
  | (x > y ) = (cmmdc (x - y) y)
  | otherwise = cmmdc x (y -x)
-}

