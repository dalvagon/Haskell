-- Leahu Vlad A6

import Prelude hiding (Either, Left, Right)
-- Ex 1.1
addThree :: (Int, Int, Int) -> Int
addThree (x, y, z) = x + y + z

-- curried addThree
addThree' :: Int -> Int -> Int -> Int
addThree' x y z = x + y + z
{-
*Main> addThree (1, 2, 3)
6
*Main> addThree' 1 2 3   
6
-}

-- Ex 2.1
f :: (Int -> Int) -> Int -> Int -> Int
f g x y | x > y = 0
f g x y = g x + f g (x + 1) y
{-
*Main> f (+ 2) 1 2
7
*Main> f (+ 1) 1 2
5
*Main> f (* 5) 1 10 
275
-}

-- Ex 2.2
comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)
{-
*Main> comp (* 4) (* 2) 3
24
*Main> comp (* 4) (+ 100) 3
412
*Main> comp (\x -> x + 2) (\x -> x ** 2) 4
18.0
-}

-- Ex 2.3 
compL :: [a -> a] -> a -> a
compL [] x = x
compL (hd : tl) x = hd (compL tl x)
{-
*Main> compL [(+2), (*2)] 2
6
*Main> compL [(+2), (*2), (\x -> x * x)] 2
10
-}

-- Ex 2.4
sumL :: [Int] -> Int
sumL [] = 0
sumL (hd : tl) = hd + sumL tl
{-
*Main> sum [1, 10, 19209, 127498]
146718
-}

-- Ex 2.5
applF :: (Int -> Int) -> [Int] -> [Int]
applF f [] = []
applF f (hd : tl) = (f hd) : applF f tl
{-
*Main> applF (\x -> x * 3 + 5) [1, 2, 3, 4] 
[8,11,14,17]
-}

-- Ex 2.6
filterL :: (a -> Bool) -> [a] -> [a]
filterL _ [] = []
filterL f (hd : tl) = if f hd then hd : filterL f tl else filterL f tl
{-
*Main> filterL (\x -> x `mod` 2 == 0) [1..20]
[2,4,6,8,10,12,14,16,18,20]
*Main> filterL (\x -> (x && True)  == True) [True, False] 
[True]
-}

-- Ex 2.7
data List = Nil | Cons Int List deriving Show
foldR :: (Int -> Int -> Int) -> Int -> List -> Int
foldR _  x Nil = x
foldR f x (Cons hd tl) = f hd (foldR f x tl)
{-
*Main> foldR (+) 5 (Cons 1(Cons 2(Cons 3(Cons 4 Nil))))
15
*Main> foldR (\x y -> x * y) 5 (Cons 1(Cons 2(Cons 3(Cons 4 Nil))))
120
-}
foldL :: (Int -> Int -> Int) -> Int -> List -> Int
foldL _  x Nil = x
foldL f x (Cons hd tl) = foldL f (f x hd) tl
{-
*Main> foldL (\x y -> x * x - y) 1 (Cons 1(Cons 2(Cons 3(Cons 4 Nil)))) 
-3
-}

-- Ex 2.8
data Tree = Leaf | Node Integer Tree Tree deriving (Show, Eq)
preOrder :: Tree -> (Integer -> Integer) -> [Integer]
preOrder Leaf _ = []
preOrder (Node value treeL treeR) f = [f value] ++ preOrder treeL f ++ preOrder treeR f

inOrder :: Tree -> (Integer -> Integer) -> [Integer]
inOrder Leaf _= []
inOrder (Node value treeL treeR) f = inOrder treeL f ++ [f value] ++ inOrder treeR f

postOrder :: Tree -> (Integer -> Integer) -> [Integer]
postOrder Leaf _ = []
postOrder (Node value treeL treeR) f = postOrder treeL f ++ postOrder treeR f ++ [f value]
{-
*Main> preOrder (Node 10 (Node 7 (Node 6 Leaf Leaf) (Node 8 Leaf Leaf)) (Node 15 Leaf Leaf)) (\x -> x + 1) 
[11,8,7,9,16]
*Main> inOrder (Node 10 (Node 7 (Node 6 Leaf Leaf) (Node 8 Leaf Leaf)) (Node 15 Leaf Leaf)) (\x -> x + 1)  
[7,8,9,11,16]
*Main> postOrder (Node 10 (Node 7 (Node 6 Leaf Leaf) (Node 8 Leaf Leaf)) (Node 15 Leaf Leaf)) (\x -> x + 1) 
[7,9,8,16,11]
-}

-- Ex 2.9
order :: Tree -> (Tree -> (Integer -> Integer) -> [Integer]) -> (Integer -> Integer) -> [Integer]
order t ord = ord t
{-
*Main> order (Node 10 (Node 7 (Node 6 Leaf Leaf) (Node 8 Leaf Leaf)) (Node 15 Leaf Leaf)) postOrder (*2)
[12,16,14,30,20]
*Main> order (Node 10 (Node 7 (Node 6 Leaf Leaf) (Node 8 Leaf Leaf)) (Node 15 Leaf Leaf)) inOrder (*4)  
[24,28,32,40,60]
-}

-- Ex 3.1
smallerList :: a -> [a] -> (a -> a -> Bool) -> [a]
smallerList _ [] _ = []
smallerList x (y : ys) f = if f y x then y : smallerList x ys f 
                                else smallerList x ys f 
biggerList :: a -> [a] -> (a -> a -> Bool) -> [a]
biggerList _ [] _ = []
biggerList x (y : ys) f = if not (f y x) then y : biggerList x ys f 
                                else biggerList x ys f 

quickSort :: [a] -> (a -> a -> Bool) -> [a]
quickSort [] _  = []
quickSort (x : xs) f = quickSort (smallerList x xs f) f ++ [x] ++ quickSort (biggerList x xs f) f
{-
*Main> quickSort [20, 3, 1, 41, 124, 8, 83, 213, 0] (\x y -> x < y)
[0,1,3,8,20,41,83,124,213]
-}

-- Ex 3.2 TODO
data Either a b = Left a | Right b deriving Show
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left a) = f a
either f g (Right b) = g b
{-
*Main> either (\x -> head x ) (+ 2)  (Left [4, 3, 2, 1] :: Either [Integer] Integer) 
4
*Main> either (\x -> head x ) (+ 2)  (Right 10 :: Either [Integer] Integer)           
12
-}

lefts :: [Either a b] -> [a]
lefts [] = []
lefts ((Left a) : xs) = a : lefts xs
lefts ((Right b) : xs) = lefts xs

rights :: [Either a b] -> [b]
rights [] = []
rights ((Right b) : xs) = b : rights xs
rights ((Left a) : xs) = rights xs
{-
*Main> lefts [Left 1, Right [1, 2, 3, 4], Left 200, Left (-1), Right [0..10]]
[1,200,-1]
*Main> rights [Left 1, Right [1, 2, 3, 4], Left 200, Left (-1), Right [0..10]]
[[1,2,3,4],[0,1,2,3,4,5,6,7,8,9,10]]
-}

isLeft :: Either a b -> Bool
isLeft (Left a) = True 
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right b) = True 
isRight _ = False
{-
*Main> isLeft (Left [1.1000])  
True
*Main> isRight (Left [1.1000]) 
False
-}

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _ = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _ = b
{-
*Main> fromRight [1, 2, 3] (Right [1, 2, 3, 4])
[1,2,3,4]
*Main> fromLeft [1, 2, 3] (Right 1)            
[1,2,3]
-}

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([], [])
partitionEithers ((Left a) : xs) = (a : fst (partitionEithers xs), snd (partitionEithers xs))
partitionEithers ((Right b) : xs) = (fst (partitionEithers xs), b : snd (partitionEithers xs))
{-
*Main> partitionEithers [Left 1, Right [1, 2, 3, 4], Left 200, Left (-1), Right [0..10]]
([1,200,-1],[[1,2,3,4],[0,1,2,3,4,5,6,7,8,9,10]])
-}

-- Ex 3.3 
data TreeAny a = L | N a (TreeAny a) (TreeAny a) deriving (Show, Eq, Ord)
{-
*Main> :t (N 100 (N 2 L L) (N 6 L L))
(N 100 (N 2 L L) (N 6 L L)) :: Num a => TreeAny a
*Main> :t (N 'a' (N 'b' L L) (N 'v' L L))
(N 'a' (N 'b' L L) (N 'v' L L)) :: TreeAny Char
-}

-- Ex 3.4 
seqSearch :: Integer -> [Integer] -> Bool 
seqSearch _ [] = False
seqSearch x xs = sum xs == foldl (\x y -> if x == y then sum xs else x) x xs
{-
*Main> seqSearch 3 [4, 5]
False
*Main> seqSearch 0 [4, 5, 3, 2]   
False
*Main> seqSearch 5 [4, 5, 3, 2]
True
-}

