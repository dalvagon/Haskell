data Tree = Leaf Int | Node Int Tree Tree deriving (Show, Eq)

t1 = Node 1 t2 t3
t1' = Node 1 t2 t3'
t2 = Node 2 (Leaf 4) (Leaf 5)
t2' = t2
t3 = Node 3 (Leaf 6) (Leaf 7)
t3' = Node 3 (Leaf 6) (Node 7 (Leaf 8) (Leaf 9))

data Dir = L | R

type Pos = [Dir] -- sinonim de tip: Pos e o lista de Dir

elemAt :: Pos -> Tree -> Tree
elemAt [] t = t
elemAt (L:pos) (Node _ l r) = elemAt pos l
elemAt (R:pos) (Node _ l r) = elemAt pos r

changeAt :: Pos -> Tree -> Int -> Tree
changeAt [] (Leaf _) i = Leaf i
changeAt [] (Node _ l r) i = Node i l r
changeAt (L:pos) (Node v l r) i = Node v (changeAt pos l i) r
changeAt (R:pos) (Node v l r) i = Node v l (changeAt pos r i)

data Crumb = LL Int Tree | RR Int Tree deriving (Show, Eq)

type Trail = [Crumb]

type Zipper = (Trail, Tree) -- reprezinta un arbore cu un nod
                            -- fixat ca nod curent

-- varianta veche: trailul este in ordine de la radacina spre elementul curent
-- zipperAt :: Pos -> Tree -> Zipper
-- zipperAt [] t = ([], t)
-- zipperAt (L:pos) (Node v l r) = let (trail, arb) = zipperAt pos l in
--                                   (LL v r : trail, arb)
-- zipperAt (R:pos) (Node v l r) = let (trail, arb) = zipperAt pos r in
--                                   (RR v l : trail, arb)
-- treeOf :: Zipper -> Tree
-- treeOf ([], t) = t
-- treeOf ((LL v r):trail, t) = Node v (treeOf (trail, t)) r
-- treeOf ((RR v l):trail, t) = Node v l (treeOf (trail, t))
-- treeOf (zipperOf t) == t ar fi un candidat bun pentru QuickCheck

-- varianta veche:
-- zipperAt [R,R] t1'
-- ([RR 1 (Node 2 (Leaf 4) (Leaf 5)),RR 3 (Leaf 6)],Node 7 (Leaf 8) (Leaf 9))

zipperAtAux :: Pos -> Tree -> Trail -> Zipper
zipperAtAux [] t a = (a, t)
zipperAtAux (L:pos) (Node v l r) a = zipperAtAux pos l ((LL v r):a)
zipperAtAux (R:pos) (Node v l r) a = zipperAtAux pos r ((RR v l):a)

zipperAt :: Pos -> Tree -> Zipper
zipperAt p t = zipperAtAux p t []

treeOf :: Zipper -> Tree
treeOf ([], t) = t
treeOf ((LL v r):trail, t) = treeOf (trail, Node v t r)
treeOf ((RR v l):trail, t) = treeOf (trail, Node v l t)

z1' = zipperAt [R,R] t1'

-- O(1)
change :: Zipper -> Int -> Zipper
change (trail, Leaf _) i = (trail, Leaf i)
change (trail, Node _ l r) i = (trail, Node i l r)

goLeft :: Zipper -> Zipper
goLeft (trail, Node v l r) = ((LL v r:trail), l)

goRight :: Zipper -> Zipper
goRight (trail, Node v l r) = ((RR v l:trail), r)

goUp :: Zipper -> Zipper
goUp (LL v r : trail, t) = (trail, Node v t r)
goUp (RR v l : trail, t) = (trail, Node v l t)

treeOf' :: Zipper -> Tree
treeOf' ([], t) = t
treeOf' zipper = treeOf' (goUp zipper)

-- lista de pozitii ar trebui sa fie in ordine inversa:
-- zipperAt' :: Pos -> Tree -> Zipper
-- zipperAt' [] t = ([], t)
-- zipperAt' (L:pos) t = goLeft (zipperAt' pos t)
-- zipperAt' (R:pos) t = goRight (zipperAt' pos t)

zipperAtAux' :: Pos -> Tree -> Pos -> Zipper
zipperAtAux' [] t [] = ([], t)
zipperAtAux' [] t (L:pos) = goLeft (zipperAtAux' [] t pos)
zipperAtAux' [] t (R:pos) = goRight (zipperAtAux' [] t pos)
zipperAtAux' (L:pos) t a = zipperAtAux' pos t (L:a)
zipperAtAux' (R:pos) t a = zipperAtAux' pos t (R:a)

zipperAt' :: Pos -> Tree -> Zipper
zipperAt' pos t = zipperAtAux' pos t []

-- Dezirabil: goUp, goLeft, goRight sa intoarca un Maybe
-- (1) fisa de laborator: o sa faceti aceasta modificare
-- Nu pot scrie goUp (goUp zipper)
-- zipper >>= goUp >>= goUp >>= goLeft >>= goRight

pred' :: Int -> Maybe Int
pred' x | x > 0 = Just (x - 1)
pred' x = Nothing

-- Just 3 >>= pred' >>= pred' >>= pred' >>= pred' >>= pred'

-- Pentru orice structura de date care tine minte
-- elemente se poate alcatui un zipper pentru structura respectiva.

-- zipper pentru liste

-- (2)
-- laborator: zipper pentru arbori in care un nod poate avea oricati fii
-- data Arb = Leaf Int | Node Int [Arb]
-- combinati zipper-ul pentru arbori cu zipper-ul pentru liste
