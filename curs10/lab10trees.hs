-- Leahu Vlad A6 - trees

data Tree a = Leaf a | Node a [Tree a] deriving (Show, Eq)

-- Deep pentru a merge la nodul curent de pe urmatorul nivel, 
-- Forward pentru a se pozitiona pe urmatorul nod fiu
data Direction = Deep | Forward

type Directions = [Direction]

data Crumb a = DeepCrumb a [Tree a] | ForwardCrumb a [Tree a] deriving Show

type Trail a = [Crumb a]

type Zipper a = (Trail a, Tree a)


{-
                    1
        2       3       4       5
    6         7   8   9 10 11   12
                             13 14 15
-}
tree :: Tree Int
tree = Node 1 [Node 2 [Leaf 6], Node 3 [Leaf 7, Leaf 8], Node 4 [Leaf 9, Leaf 10, Leaf 11], Node 5 [Node 12 [Leaf 13, Leaf 14, Leaf 15]]]



zipperAtAux :: Directions -> Tree a -> Trail a -> Zipper a
zipperAtAux [] tree trail = (trail, tree)
zipperAtAux (Deep:directions) (Node v (tree:trees)) trail = zipperAtAux directions tree (DeepCrumb v trees:trail)
zipperAtAux (Forward:directions) (Node v1 ((Node v2 trees2):trees1)) trail = zipperAtAux directions (Node v1 trees1) (ForwardCrumb v2 trees2:trail)
zipperAtAux (Forward:directions) (Node v1 ((Leaf v2):trees1)) trail = zipperAtAux directions (Node v1 trees1) (ForwardCrumb v2 []:trail)

zipperAt :: Directions -> Tree a -> Zipper a
zipperAt directions tree = zipperAtAux directions tree []


treeOf :: Zipper a -> Tree a
treeOf ([], tree) = tree
treeOf (DeepCrumb v trees:trail, tree) = treeOf (trail, Node v (tree:trees))
treeOf (ForwardCrumb v1 []:trail, Node v2 trees2) = treeOf (trail, Node v2 (Leaf v1:trees2))
treeOf (ForwardCrumb v1 trees1:trail, Node v2 trees2)=  treeOf (trail, Node v2 (Node v1 trees1:trees2))
{-
treeOf $ zipperAt [Forward, Forward, Forward, Deep, Deep, Forward, Forward, Deep] tree
Node 1 [Node 2 [Leaf 6],Node 3 [Leaf 7,Leaf 8],Node 4 [Leaf 9,Leaf 10,Leaf 11],Node 5 [Node 12 [Leaf 13,Leaf 14,Leaf 15]]]
-}


change :: a -> Zipper a -> Maybe (Zipper a)
change newVal (trail, Leaf _) = Just (trail, Leaf newVal)
change newVal (trail, Node _ trees) = Just (trail, Node newVal trees)


zipper :: Zipper Int
zipper = zipperAt [Forward, Forward, Forward, Deep, Deep, Forward, Forward, Deep] tree


-- goUp merge inapoi pe acelasi nivel daca intalnesc un ForwardCrumb
-- sau pe nivelul urmator daca intalnesc un DeepCrumb
goUp :: Zipper a -> Maybe (Zipper a)
goUp ([], _) = Nothing
goUp (ForwardCrumb v1 []:trail, Node v2 trees2) = Just (trail, Node v2 (Leaf v1:trees2))
goUp (ForwardCrumb v1 trees1:trail, Node v2 trees2) = Just (trail, Node v2 (Node v1 trees1:trees2))
goUp (DeepCrumb v trees:trail, tree) = Just (trail, Node v (tree:trees))

goDeep :: Zipper a -> Maybe (Zipper a)
goDeep (_, Leaf _) = Nothing
goDeep (trail, Node v (tree:trees)) = Just (DeepCrumb v trees:trail, tree)

goForward :: Zipper a -> Maybe (Zipper a)
goForward (_, Node _ []) = Nothing
goForward (trail, Node v1 ((Node v2 trees2):trees1)) = Just (ForwardCrumb v2 trees2:trail, Node v1 trees1)
goForward (trail, Node v1 ((Leaf v2):trees1)) = Just (ForwardCrumb v2 []:trail, Node v1 trees1)

{-
*Main> Just zipper >>= goUp >>= goUp >>= goUp >>= goUp >>= goUp >>= goUp >>= goUp >>= goUp >>= goForward >>= goDeep >>= change 100 >>= goUp >>= goUp >>= change 200 >>= goDeep >>= change 100 >>= goUp
Just ([],Node 200 [Node 100 [Leaf 6],Node 100 [Leaf 7,Leaf 8],Node 4 [Leaf 9,Leaf 10,Leaf 11],Node 5 [Node 12 [Leaf 13,Leaf 14,Leaf 15]]])
-}