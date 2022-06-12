-- Leahu Vlad A6 - binary trees

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)

data Direction = L | R deriving Show

type Directions = [Direction]

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show

type Trail a = [Crumb a]

type Zipper a = (Trail a, Tree a)


tree :: Tree Int
tree = Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Node 3 (Leaf 6) (Node 7 (Leaf 8) (Leaf 9)))


zipperAtAux :: Directions -> Tree a -> Trail a -> Zipper a
zipperAtAux [] tree trail = (trail, tree)
zipperAtAux (L:directions) (Node v l r) trail = zipperAtAux directions l ((LeftCrumb v r):trail)
zipperAtAux (R:directions) (Node v l r) trail = zipperAtAux directions r ((RightCrumb v l):trail)

zipperAt :: Directions -> Tree a -> Zipper a
zipperAt directions tree = zipperAtAux directions tree []

treeOf :: Zipper a -> Tree a
treeOf ([], t) = t
treeOf ((LeftCrumb v r):trail, t) = treeOf (trail, Node v t r)
treeOf ((RightCrumb v l):trail, t) = treeOf (trail, Node v l t)
{-
*Main> treeOf $ zipperAt [R, R] tree
Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Node 3 (Leaf 6) (Node 7 (Leaf 8) (Leaf 9)))
-}

-- EX 1
zipper :: Zipper Int
zipper = zipperAt [R, R] tree

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (_, Leaf _) = Nothing
goLeft (trail, Node v l r) = Just (LeftCrumb v r:trail, l)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (_, Leaf _) = Nothing
goRight (trail, Node v l r) = Just (RightCrumb v l:trail, r)

goUp :: Zipper a -> Maybe (Zipper a)
goUp ([], _) = Nothing
goUp (LeftCrumb v r:trail, tree) = Just (trail, Node v tree r)
goUp (RightCrumb v l:trail, tree) = Just (trail, Node v l tree)

change :: a -> Zipper a -> Maybe (Zipper a)
change newVal (trail, Leaf _) = Just (trail, Leaf newVal)
change newVal (trail, Node _ l r) = Just (trail, Node newVal l r)


-- EX 2
{-
*Main> Just zipper >>= goUp >>= goUp >>= goLeft >>= goRight                    
Just ([RightCrumb 2 (Leaf 4),LeftCrumb 1 (Node 3 (Leaf 6) (Node 7 (Leaf 8) (Leaf 9)))],Leaf 5)  
*Main> Just zipper >>= goUp >>= goUp >>= goLeft >>= goRight >>= goRight
Nothing
*Main> Just zipper >>= goUp >>= goUp >>= goLeft >>= goRight >>= change 100
Just ([RightCrumb 2 (Leaf 4),LeftCrumb 1 (Node 3 (Leaf 6) (Node 7 (Leaf 8) (Leaf 9)))],Leaf 100)
*Main> Just zipper >>= goUp >>= goUp >>= goLeft >>= change 100 >>= goUp >>= goRight >>= goLeft >>= change 100 >>= goUp >>= goUp
Just ([],Node 1 (Node 100 (Leaf 4) (Leaf 5)) (Node 3 (Leaf 100) (Node 7 (Leaf 8) (Leaf 9))))
-}



