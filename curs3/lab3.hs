-- Leahu Vlad Iulius A6
import Prelude hiding (exp, div, mod)
-- Ex 1.2
data MobileDevice = Smartphone Color
                  | Laptop Color
                  | Tablet Integer String Color
                  deriving (Show)
-- Ex 1.3
data Color = Blue
           | Black
           | Red
           | Yellow
           deriving (Show)

{-
    Main> :t Smartphone Blue
    Smartphone Blue :: MobileDevice
    *Main> :t Laptop Black   
    Laptop Black :: MobileDevice
    *Main> :t Tablet 15 "Asus"
    Tablet 15 "Asus" :: Color -> MobileDevice
-}

-- Ex 1.4
desc :: MobileDevice -> Color
desc (Smartphone col) = col
desc (Laptop col) = col
desc (Tablet _ _ col) = col
{-
    Main> desc (Laptop Blue)
    Blue
    *Main> desc (Tablet 15 "Asus" Blue)
    Blue
-}

{-
Trees in which each node has 2 descendats:
Ex 2.1
data Tree = Leaf Integer | Node Integer Tree Tree deriving (Show, Eq)
{-
    :t (Node 10 (Node 7 (Leaf 5) (Leaf 7)) (Node 11 (Leaf 10) (Leaf 12)))
    (Node 10 (Node 7 (Leaf 5) (Leaf 7)) (Node 11 (Leaf 10) (Leaf 12)))
    :: Tree
-}

-- Ex 2.2

minTree :: Tree -> Integer
minTree (Leaf value) = value
minTree (Node valueR treeL treeR) = min valueR (min (minTree treeL) (minTree treeR))

maxTree :: Tree -> Integer
maxTree (Leaf value) = value
maxTree (Node valueR treeL treeR) = max valueR (max (maxTree treeL) (maxTree treeR))


isBST :: Tree -> Bool
isBST (Leaf value) = True
isBST (Node value (Leaf valueL) (Leaf valueR))
    | valueL >= value || valueR <= value = False
    | otherwise = True
isBST (Node value (Leaf valueL) (Node valueR treeL' treeR'))
    | valueL >= value || valueR <= value
                      || minTree (Node valueR treeL' treeR') <= value = False
    | otherwise = isBST (Node valueR treeL' treeR')
isBST (Node value (Node valueL treeL treeR) (Leaf valueR))
    | valueL >= value || valueR <= value
                      || maxTree (Node valueL treeL treeR) >= value  = False
    | otherwise = isBST (Node valueL treeL treeR)
isBST (Node value (Node valueL treeL treeR) (Node valueR treeL' treeR'))
    | valueL >= value || valueR  <= value
                      || maxTree (Node valueL treeL treeR) >= value
                      ||  minTree (Node valueR treeL' treeR') <= value = False
    | otherwise = isBST (Node valueL treeL treeR) && isBST (Node valueR treeL' treeR')

{-
    Main> isBST (Node 10 (Node 7 (Leaf 5) (Leaf 7)) (Node 11 (Leaf 10) (Leaf 12)))
    False
    *Main> isBST (Node 10 (Node 7 (Leaf 5) (Leaf 8)) (Node 12 (Leaf 11) (Leaf 13)))
    True
    *Main> isBST (Node 6 (Node 3 (Leaf 1) (Leaf 5)) (Node 9 (Leaf 7) (Leaf 11)))
    True
-}

-- Ex 2.3
data ResultS = NotBST | Valid Bool deriving Show


search' :: Tree -> Integer -> ResultS
search' tree _ | not (isBST tree) = NotBST
search' (Leaf value) searchValue | (==) value searchValue = Valid True 
                                 | otherwise = Valid False
search' (Node value treeL treeR) searchValue
    | value == searchValue = Valid True
    | otherwise = if searchValue < value then search' treeL searchValue
                    else search' treeR searchValue

aux :: ResultS -> String 
aux NotBST = "The tree is not a binary search tree"
aux (Valid boolean) | (==) boolean True = "Value found"
                    | otherwise = "Value not found"

search :: Tree -> Integer -> String 
search tree searchvalue = aux (search' tree searchvalue)

Main> search (Node 6 (Node 3 (Leaf 1) (Leaf 5)) (Node 9 (Leaf 7) (Leaf 1))) 100
"The tree is not binary search tree"
*Main> search (Node 6 (Node 3 (Leaf 1) (Leaf 5)) (Node 9 (Leaf 7) (Leaf 11))) 100
"Value not found"
*Main> search (Node 6 (Node 3 (Leaf 1) (Leaf 5)) (Node 9 (Leaf 7) (Leaf 11))) 11
"Value found"
-}




--Ex 2.1
data Tree = Leaf | Node Integer Tree Tree deriving (Show, Eq)
{-
    Main> :t (Node 2 Leaf Leaf)
    (Node 2 Leaf Leaf) :: Tree
    *Main> Leaf
    Leaf
    *Main> :t (Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf Leaf)))
    (Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf Leaf)))
    :: Tree
-}

-- Ex 2.2

data MM = Valid Integer | ThisIsLeaf deriving Show

intg :: MM -> Integer
intg (Valid value) = value
intg ThisIsLeaf = -1

minTree :: Tree -> MM -- minimum value in a binary tree
minTree Leaf = ThisIsLeaf
minTree (Node value Leaf Leaf) = Valid value
minTree (Node value Leaf (Node value' treeL treeR)) = Valid (min value (intg (minTree(Node value' treeL treeR))))
minTree (Node value (Node value' treeL treeR) Leaf) = Valid (min value (intg (minTree(Node value' treeL treeR))))
minTree (Node value (Node value' treeL' treeR') (Node value'' treeL'' treeR''))
        = Valid (min value (min (intg (minTree(Node value' treeL' treeR'))) (intg (minTree(Node value'' treeL' treeR')))))

maxTree :: Tree -> MM -- maxumun value in a binary tree
maxTree Leaf = ThisIsLeaf
maxTree (Node value Leaf Leaf) = Valid value
maxTree (Node value Leaf (Node value' treeL treeR)) = Valid (max value (intg (maxTree(Node value' treeL treeR))))
maxTree (Node value (Node value' treeL treeR) Leaf) = Valid (max value (intg (maxTree(Node value' treeL treeR))))
maxTree (Node value (Node value' treeL' treeR') (Node value'' treeL'' treeR''))
        = Valid (max value (max (intg (minTree(Node value' treeL' treeR'))) (intg (minTree(Node value'' treeL' treeR')))))

isBST :: Tree -> Bool
isBST Leaf = True
isBST (Node value Leaf Leaf) = True
isBST (Node value Leaf (Node valueR treeL' treeR'))
    | valueR <= value || intg (minTree (Node valueR treeL' treeR')) <= value = False
    | otherwise = isBST (Node valueR treeL' treeR')
isBST (Node value (Node valueL treeL treeR) Leaf)
    | valueL >= value || intg (maxTree (Node valueL treeL treeR)) >= value  = False
    | otherwise = isBST (Node valueL treeL treeR)
isBST (Node value (Node valueL treeL treeR) (Node valueR treeL' treeR'))
    | valueL >= value || valueR  <= value
                      || intg (maxTree (Node valueL treeL treeR)) >= value
                      || intg (minTree (Node valueR treeL' treeR')) <= value = False
    | otherwise = isBST (Node valueL treeL treeR) && isBST (Node valueR treeL' treeR')

{-
    Main> isBST (Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf Leaf)))  
    True
    *Main> isBST (Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 10 Leaf Leaf))) 
    False
-}

-- Ex 2.3
data ResultS = NotBST | Valid' Bool deriving Show

search' :: Tree -> Integer -> ResultS
search' tree _ | not (isBST tree) = NotBST
search' Leaf searchValue = Valid' False
search' (Node value treeL treeR) searchValue
    | value == searchValue = Valid' True
    | otherwise = if searchValue < value then search' treeL searchValue
                    else search' treeR searchValue

aux :: ResultS -> String
aux NotBST = "The tree is not a binary search tree"
aux (Valid' boolean) | (==) boolean True = "Value found"
                     | otherwise = "Value not found"

search :: Tree -> Integer -> String
search tree searchvalue = aux (search' tree searchvalue)
{-
    Main> search (Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf Leaf))) 10
    "Value not found"
    *Main> search (Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf Leaf))) 50
    "Value found"
    *Main> search (Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf Leaf))) 70
    "Value found"
-}

insert :: Tree -> Integer -> Tree
insert Leaf insValue = Node insValue Leaf Leaf
insert (Node value treeL treeR) insValue
    | (==) insValue value = Node value treeL treeR
    | insValue < value = Node value (insert treeL insValue) treeR
    | otherwise = Node value treeL (insert treeR insValue)

{-
    Main> insert  (Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf Leaf)))  1000
                   Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf (Node 1000 Leaf Leaf)))
    *Main> insert  (Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf Leaf))) 70
                    Node 100 (Node 50 (Node 20 Leaf Leaf) (Node 70 Leaf Leaf)) (Node 120 (Node 110 Leaf Leaf) (Node 130 Leaf Leaf))
-}

-- Ex 2.5
{-
    Min, max - deja calculate la ex Ex 2.2
-}

-- Ex 2.6 TODO
-- Ex 2.7 TODO

--Ex 2.8
preOrder :: Tree -> [Integer]
preOrder Leaf = []
preOrder (Node value treeL treeR) = [value] ++ preOrder treeL ++ preOrder treeR

inOrder :: Tree -> [Integer]
inOrder Leaf = []
inOrder (Node value treeL treeR) = inOrder treeL ++ [value] ++ inOrder treeR

postOrder :: Tree -> [Integer]
postOrder Leaf = []
postOrder (Node value treeL treeR) = postOrder treeL ++ postOrder treeR ++ [value]
{-
    Main> preOrder (Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 7 Leaf Leaf)))
    [1,2,4,5,3,6,7]
    *Main> inOrder (Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 7 Leaf Leaf))) 
    [4,2,5,1,6,3,7]
    *Main> postOrder (Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 7 Leaf Leaf)))
    [4,5,2,6,7,3,1]
-}

-- Ex 3
height :: Tree -> Integer
height Leaf = -1
height (Node _ treeL treeR) = 1 + max (height treeL) (height treeR)
{-
    Main> height (Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 7 (Node 7 (Node 7 Leaf Leaf) Leaf) (Node 4 Leaf Leaf))))  
    5
-}

isAVL :: Tree -> Bool
isAVL Leaf = True
isAVL (Node _ treeL treeR) = isAVL treeL && isAVL treeR
                             && height treeL - height treeR >= (-1)
                             && height treeL - height treeR <=1
{-
    Main> isAVL (Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 7 Leaf Leaf)))
    True   
    *Main> isAVL (Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 7 (Node 7 (Node 7 Leaf Leaf) Leaf) (Node 4 Leaf Leaf))))   
    False
-}

-- rotateLeft :: Tree -> Tree
-- rotateLeft Leaf = Leaf
-- rotateLeft (Node value Leaf Leaf) = Node value Leaf Leaf
-- rotateLeft (Node value (Node valueL treeL treeR) Leaf) = Node value (Node valueL treeL treeR) Leaf
-- rotateLeft (Node value Leaf (Node valueR treeL treeR)) = Node valueR (Node value treeL Leaf) treeR
-- rotateLeft (Node value (Node valueL treeL treeR) (Node valueR treeL' treeR'))
--             = Node valueR (Node value (Node valueL treeL treeR) treeL') treeR'



-- Ex 4
data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)
{-
    Main> add Zero Zero
    Zero
    *Main> add Zero (Succ Zero)
    Succ Zero
    *Main> add (Succ (Succ Zero)) (Succ Zero)
    Succ (Succ (Succ Zero))
-}

mult :: Nat -> Nat -> Nat
mult Zero y = Zero
mult (Succ x) y = add y (mult x y)
{-
    Main> mult (Succ (Succ (Succ Zero))) (Succ (Succ (Succ(Zero))))
    Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
-}

exp :: Nat -> Nat -> Nat
exp x Zero = Succ Zero
exp x (Succ y) = mult x (exp x y)
{-
    Main> exp (Succ (Succ (Succ Zero))) (Succ (Succ (Succ(Zero))))
    Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ 
    (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ 
    (Succ (Succ (Succ (Succ (Succ Zero))))))))))))))))))))))))))
-}

comp :: Nat -> Nat -> Bool
comp Zero (Succ y) = True
comp (Succ x) Zero = False
comp (Succ x) (Succ y) = comp x y
comp _ _ = False
{-
    Main> comp Zero Zero
    False
    *Main> comp Zero (Succ Zero)
    True
    *Main> comp (Succ (Succ Zero)) (Succ Zero)
    False
-}

data ErrorNat = Val Nat | Error deriving Show

dif :: Nat -> Nat -> ErrorNat
dif Zero (Succ y) = Error
dif x Zero = Val x
dif (Succ x) (Succ y) = dif x y

{-
    Main> dif Zero (Succ Zero)
    Error
    *Main> dif (Succ Zero) (Succ Zero)
    Val Zero
-}

toNat :: ErrorNat -> Nat
toNat (Val nat) = nat
toNat Error = Zero

div :: Nat -> Nat -> ErrorNat
div x Zero = Error
div x y | not (comp x y) = Val (add (Succ Zero) (toNat (div (toNat (dif x y)) y)))
        | otherwise = Val Zero
{-
    Main> div Zero Zero
    Error
    *Main> div Zero (Succ Zero)
    Val Zero
    *Main> div (Succ Zero) (Succ Zero)
    Val (Succ Zero)
    *Main> div (Succ (Succ (Succ (Succ Zero)))) (Succ (Succ Zero))
    Val (Succ (Succ Zero))
-}

mod :: Nat -> Nat -> ErrorNat
mod x Zero = Error
mod x y | not (comp x y) = Val (toNat (mod (toNat (dif x y)) y))
        | otherwise = Val x
{-
    Main> mod (Succ (Succ (Succ (Succ Zero)))) (Succ (Succ Zero))
    Val Zero
    *Main> mod (Succ (Succ (Succ (Succ Zero)))) (Succ (Succ (Succ Zero)))
    Val (Succ Zero)
-}

convert :: Nat -> Integer
convert Zero = 0
convert (Succ x) = 1 + convert x
{-
    Main> convert Zero
    0
    *Main> convert (Succ (Succ Zero))
    2
-}

convert' :: Integer -> ErrorNat
convert' 0 = Val Zero
convert' x | x < 0 = Error
           | otherwise = Val (Succ (toNat (convert' (x - 1))))

{-
    Main> convert' (-1) 
    Error
    *Main> convert' 10  
    Val (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))
-}

-- Ex 6.1
data List = Nil | Cons Integer List deriving Show

-- Ex 6.2
data ResS = Found | NotFound deriving Show
searchList :: List -> Integer -> ResS
searchList Nil searchValue = NotFound
searchList (Cons hd tl) searchValue | hd == searchValue = Found
                                    | otherwise = searchList tl searchValue

{-
    Main> searchList  (Cons 1 (Cons 2 Nil)) 10
    NotFound
    *Main> searchList  (Cons 1 (Cons 2 Nil)) 1 
    Found
-}

-- Ex 6.3
addAtEnd :: List -> Integer -> List
addAtEnd Nil addValue = Cons addValue Nil
addAtEnd (Cons hd tl) addValue = Cons hd (addAtEnd tl addValue)
{-
    Main> addAtEnd (Cons 1 (Cons 2 (Cons 3 Nil))) 4
    Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
-}

-- Ex 6.4
addAtStart :: List -> Integer -> List
addAtStart Nil addValue = Cons addValue Nil
addAtStart (Cons hd tl) addValue = Cons addValue (Cons hd tl)
{-
    Main> addAtStart  (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) 4
    Cons 4 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
-}

-- Ex 6.5
data ErrorList' = Errorr | Good List deriving Show

toList' :: ErrorList' -> List
toList' Errorr = Nil
toList' (Good list) = list

listLength :: List -> Integer
listLength Nil = 0;
listLength (Cons hd tl) = 1 + listLength tl

--  addAtIndex list addValue index
addAtIndex :: List -> Integer -> Integer -> ErrorList'
addAtIndex list _ index | listLength list + 1 < index || index < 1 = Errorr
addAtIndex Nil addValue _ = Good (Cons addValue Nil)
addAtIndex (Cons hd tl) addValue 1 = Good (Cons addValue (Cons hd tl))
addAtIndex (Cons hd tl) addValue index = Good (Cons hd (toList' (addAtIndex tl addValue (index - 1))))
{-
    Main> addAtIndex  (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) 5 5
    Good (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))
    *Main> addAtIndex (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))) 2 100
    Errorr
    *Main> addAtIndex (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))) 100 2
    Good (Cons 1 (Cons 100 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))))
-}


-- Ex 6.6, 6.7
data ErrorList = Errorr' | Val' Integer deriving Show

toInt :: ErrorList -> Integer
toInt Errorr' = -1
toInt (Val' numb) = numb

maxList :: List -> ErrorList
maxList Nil = Errorr'
maxList (Cons hd Nil) = Val' hd
maxList (Cons hd tl) = Val' (max hd (toInt (maxList tl)))

minList :: List -> ErrorList
minList Nil = Errorr'
minList (Cons hd Nil) = Val' hd
minList (Cons hd tl) = Val' (min hd (toInt (minList tl)))

{-
    Main> maxList  (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))
    Val' 5
    maxList (Cons (-2) Nil)      
    Val' (-2)
-}

-- Ex 6.8

concatLists :: List -> List -> List
concatLists Nil l = l
concatLists l Nil = l
concatLists (Cons hd Nil) (Cons hd' tl') = Cons hd (Cons hd' tl')
concatLists (Cons hd tl) l = Cons hd (concatLists tl l)
{-
    Main> concatLists (Cons 1 (Cons 2 Nil)) (Cons 3 Nil)                                    
    Cons 1 (Cons 2 (Cons 3 Nil))
    *Main> concatLists (concatLists (Cons 1 (Cons 2 Nil)) (Cons 3 Nil)) (Cons 4 (Cons 5 Nil))
    Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
-}

convertList :: List -> [Integer]
convertList Nil = []
convertList (Cons hd tl) = hd : convertList tl
{-
    Main> convertList (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))
    [1,2,3,4,5]
-}

convertList' :: [Integer] -> List
convertList' [] = Nil
convertList' (x : xs) = Cons x (convertList' xs)
{-
    Main> convertList' [1,2,3,4,5]
    Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
-}



-- Ex 8 
data Boolean = T | F | Not Boolean | And Boolean Boolean | Or Boolean Boolean deriving (Show, Eq)
simpl :: Boolean -> Boolean
simpl T = T
simpl F = F
simpl (Not T) = F
simpl (Not F) = T
simpl (Not b) = Not (simpl b)
simpl (And T b) = simpl b
simpl (And b T) = simpl b
simpl (And _ F) = F
simpl (And F _) = F
simpl (And b b') = And (simpl b) (simpl b')
simpl (Or F b) = simpl b
simpl (Or b F) = simpl b
simpl (Or b b') = Or (simpl b) (simpl b')
