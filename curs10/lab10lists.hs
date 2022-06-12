-- Leahu Vlad A6 - lists

-- EX 3

data List a = Nil | Cons a (List a) deriving (Show, Eq)

data Direction = Fwd

type Directions = [Direction]

type Zipper a = ([a], List a)


list :: List Int
list = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))


zipperAtAux :: Directions -> List a -> [a] -> Zipper a
zipperAtAux [] list trail = (trail, list)
zipperAtAux (Fwd:directions) (Cons v list) trail= zipperAtAux directions list (v:trail)

zipperAt :: Directions -> List a -> Zipper a
zipperAt directions list = zipperAtAux directions list []

listOf :: Zipper a -> List a
listOf ([], list) = list
listOf (v:trail, list) = listOf (trail, Cons v list)  

{-
*Main> listOf $ zipperAt [Fwd, Fwd] list
Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
-}

-- EX 3
zipper :: Zipper Int
zipper = zipperAt [Fwd, Fwd] list

change :: a -> Zipper a -> Maybe (Zipper a)
change newVal (trail, Nil) = Nothing 
change newVal (trail, Cons _ list) = Just (trail, Cons newVal list)

goFwd :: Zipper a -> Maybe (Zipper a)
goFwd (_, Nil) = Nothing 
goFwd (trail, Cons v list) = Just (v:trail, list)

goBwd :: Zipper a -> Maybe (Zipper a)
goBwd ([], _) = Nothing 
goBwd (v:trail, list) = Just (trail, Cons v list) 

{-
*Main> zipper
([2,1],Cons 3 (Cons 4 Nil))
*Main> Just zipper >>= goBwd >>= goBwd >>= change 2 >>= goFwd >>= change 3 >>= goFwd >>= change 4 >>= goBwd >>= goBwd
Just ([],Cons 2 (Cons 3 (Cons 4 (Cons 4 Nil))))
-}
