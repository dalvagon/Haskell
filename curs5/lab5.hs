-- Leahu Vlad A6
-- Ex 1
-- :i Show, Eq, Ord, Read, Enum, Num
-- hackage.haskell.org/

-- Ex 2
-- lista tipurilor se gaseste in campul Instances




-- Ex 3 
-- data Nat' = Cons [Bool]
data Nat' = Zero' | Double Nat' | DoubleAddOne Nat' deriving Show
four = Double (Double (DoubleAddOne Zero'))
seven = DoubleAddOne (DoubleAddOne (DoubleAddOne Zero'))

convert10 :: Nat' -> Integer
convert10 Zero' = 0
convert10 (Double x) = 2 * convert10 x
convert10 (DoubleAddOne x) = 1 + 2 * convert10 x

convert2 :: Integer  -> Nat'
convert2 0 = Zero'
convert2 x = if mod x 2 == 1 then DoubleAddOne (convert2 (div x 2)) else  Double (convert2 (div x 2))
{-
*Main> convert10 seven
7
*Main> convert10 four
4
*Main> convert10 (convert2 5)
5
*Main> convert10 (convert2 100)
100
-}

-- Ex 4
instance Eq Nat' where
    (==) Zero' Zero' = True
    (==) (Double x) (Double y) = (==) x y
    (==) (DoubleAddOne x) (DoubleAddOne y) = (==) x y
    (==) _ _ = False
{-
*Main> four == seven  
False
*Main> four == four  
True
-}

instance Ord Nat' where
    (<=) x y = (<=) (convert10 x) (convert10 y)
{-
*Main> (<) four four
False
*Main> (<=) four four
True
*Main> (<=) four seven
True
*Main> (>=) four seven
False
-}

instance Enum Nat' where
    toEnum x = convert2 (toInteger x)
    fromEnum x = fromInteger (convert10 x)
{-
*Main> convert10 (succ four)
5
*Main> convert10 (pred seven)
6
-}

instance Num Nat' where
    (+) x y = convert2 (convert10 x + convert10 y)
    (*) x y = convert2 (convert10 x * convert10 y)
    abs x = x
    signum Zero' = Zero'
    signum _ = 1
    fromInteger x = convert2 x
    (-) x y = if (<) x y then convert2 9999 else
                               convert2 (convert10 x - convert10 y)
{-
*Main> seven - four
DoubleAddOne (DoubleAddOne Zero')
*Main> seven - seven
Zero'
*Main> four - seven
DoubleAddOne (DoubleAddOne (DoubleAddOne (DoubleAddOne (Double (Double (Double (Double (DoubleAddOne (DoubleAddOne (DoubleAddOne (Double (Double (DoubleAddOne Zero')))))))))))))
-}

instance Real Nat' where
    toRational x = toRational (convert10 x)

instance Integral Nat' where
    quotRem x y = (convert2 (div (convert10 x) (convert10 y)),
                   convert2 (mod (convert10 x) (convert10 y)))
    toInteger x = toInteger (convert10 x)


-- Ex 5
data Complex a = Complex a a deriving Show

instance (Num a,  Floating a) => Num (Complex a) where
    (+) (Complex x y) (Complex x' y') = Complex (x + x') (y + y')
    (*) (Complex x y) (Complex x' y') = Complex (x * x' - y * y') (x * y' + x' * y)
    abs (Complex x y) = Complex (sqrt (x * x + y * y)) 0
    signum (Complex x y) = abs (Complex x y)
    fromInteger x = Complex (fromInteger x) 0
    (-) (Complex x y) (Complex x' y') = Complex (x - x') (y - y')

-- Ex 6
class (Eq a) => MyOrd a where
    lt :: a -> a -> Bool
    gt :: a -> a -> Bool
    lte :: a -> a -> Bool
    gte :: a -> a -> Bool
    mx :: a -> a -> a
    mn :: a -> a -> a
    lt x y = not (gte x y)
    gt x y = not (lte x y)
    mx x y = if lt x y then y else x
    mn x y = if lt x y then x else y

instance MyOrd Int where
    lte x y = x <= y
    gte x y = x >= y

myOne = 1 :: Int
mySeven = 7 :: Int
myFour = 4 :: Int
myTen = 10 :: Int
myTwenty = 20 :: Int
{-
*Main> mx mySeven  myFour 
7
*Main> lt mySeven myFour  
False
*Main> mx mySeven  myFour 
7
*Main> lte myFour myFour 
True
-}

instance (MyOrd a) => MyOrd [a] where
    lte [] [] = True
    lte [] _ = False
    lte _ [] = True
    lte (x : xs) (y : ys) = lte x y || (lte x y && lte xs ys)
    gte [] [] = True
    gte [] _ = True
    gte _ [] = False
    gte (x : xs) (y : ys) = gte x y || (gte x y && gte xs ys)

{-
*Main> lte [myFour, mySeven] [mySeven, myFour, mySeven] 
True
*Main> gt [myFour, mySeven] [mySeven, myFour, mySeven] 
False
-}

smallerList :: MyOrd a => a -> [a] -> [a]
smallerList _ [] = []
smallerList x (y : ys) = if lt y x then y : smallerList x ys
                                else smallerList x ys
biggerList :: MyOrd a => a -> [a] -> [a]
biggerList _ [] = []
biggerList x (y : ys) = if not (lt y x) then y : biggerList x ys
                                else biggerList x ys

sort :: MyOrd a => [a] -> [a]
sort []  = []
sort (x : xs) = sort (smallerList x xs) ++ [x] ++ sort (biggerList x xs)
{-
*Main> sort [myTwenty, mySeven, myTen, myFour, myOne, myFour]
[1,4,4,7,10,20]
-}

-- Ex 7
{-
data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)
zero = Zero
one = Succ zero
two = Succ one
three = Succ two
*Main> Zero >= one
False
*Main> two == two 
True

data Nat = Succ Nat | Zero deriving (Show, Eq, Ord)
*Main> Zero >= one
True

Se observa ca operatiile de comparare nu dau rezultate corecte intotdeauna
-}

-- Ex 8
data Nat = Succ Nat | Zero
zero = Zero
one = Succ zero
two = Succ one
three = Succ two

instance Show Nat where
    show Zero = ['o']
    show (Succ x) = "(s " ++ show x ++ ")"
{-
*Main> two
(s (s o))
*Main> three
(s (s (s o)))
-}

-- Ex 9
instance Ord Nat where
    (<=) Zero Zero = True
    (<=) Zero _ = True
    (<=) _ Zero = False
    (<=) (Succ x) (Succ y) = (<=) x y

-- Ex 10
data Arb' = Leaf' | Node' Integer Arb' Arb'
arb' = Node' 1 (Node' 4 (Node' 6 (Node' 100 Leaf' Leaf') Leaf') Leaf') (Node' 2 Leaf' (Node' 5 Leaf' Leaf'))

instance Show Arb' where
    show Leaf' = "()"
    show (Node' x left right) = "(" ++ show x ++ show left ++ show right ++ ")"
{-
*Main> show arb'
"(1(4(6(100()())())())(2()(5()())))"
-}

-- Ex 11
data Arb a = Leaf | Node a (Arb a) (Arb a)
arb = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

instance (Show a) => Show (Arb a) where
    show Leaf = "()"
    show (Node x left right) = "(" ++ show x ++ show left ++ show right ++ ")"
-- Avem nevoie ca a sa faca parte din Show pentru a putea fi transformat in String si afisat

-- Ex 12
instance Eq Nat where
    (==) Zero Zero = True
    (==) (Succ x) (Succ y) = (==) x y
    (==) _ _ = False

-- Ex 13

instance (Eq a) => Eq (Arb a) where
    (==) Leaf Leaf = True
    (==) (Node x lx rx) (Node y ly ry) = (x == y) && (==) lx ly && (==) rx ry
    (==) _ _ = False

{-
*Main> (==) arb (Node 2 Leaf arb) 
False
*Main> (==) arb arb              
True
-}

-- Ex 14
class Pretty a where
    prettyPrint :: a -> String

instance Pretty Nat where
    prettyPrint x = show x
    
instance (Show a) => Pretty (Arb a) where
    prettyPrint x = show x

{-
*Main> prettyPrint three
"(s (s (s o)))"
*Main> prettyPrint arb  
"(1(2()())(3(4()())()))"
-}

-- Ex 15
class MyNum a where
    toInt :: a -> Int

instance MyNum Nat where
    toInt Zero = 0
    toInt (Succ x) = 1 + toInt x 

-- Ex 16
instance Num Nat where
    (+) Zero y = y
    (+) x Zero = x
    (+) (Succ x) y = Succ ((+) x y)
    (*) Zero _ = Zero
    (*) _ Zero = Zero
    (*) x (Succ y) = (+) x ((*) x y)
    abs x = x
    signum x = Succ Zero
    fromInteger 0 = Zero
    fromInteger x = Succ (fromInteger (x - 1))
    (-) x Zero = x
    (-) Zero y = Zero
    (-) (Succ x) (Succ y) = (-) x y

{-
*Main> fromInteger 6
6
*Main> (fromInteger 6) :: Nat
(s (s (s (s (s (s o))))))
-}

-- Ex 17
data List a = Nil | Cons a (List a) deriving Show
instance (Eq a) => Eq (List a) where
    (==) Nil Nil = True 
    (==) Nil _ = False 
    (==) _ Nil = False 
    (==) (Cons x lx) (Cons y ly) = (==) x y && (==) lx ly

l1 = Cons 1 (Cons 2 Nil)
l2 = Cons 1 (Cons 2 (Cons 3 Nil))
{-
*Main> (==) l1 l2
False
*Main> (==) l1 l1
True
-}

-- Ex 18
instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x lx) = Cons (f x) (fmap f lx)

{-
*Main> fmap (\x -> x + 2) l1
Cons 3 (Cons 4 Nil)
*Main> fmap (\x -> x + 2) l2
Cons 3 (Cons 4 (Cons 5 Nil))
-}