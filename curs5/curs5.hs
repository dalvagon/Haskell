-- Clase de tipuri

-- Clasa de tipuri = multime de tipuri
--                   cu cateva caracteristici similare

data Dow = Mon | Tue | Wed | Thu | Fri | Sun | Sat deriving (Bounded, Enum, Read)

-- Show este clasa tipurilor care pot fi transformate
-- in siruri de caractere

-- "a" face parte din clasa Show
-- "a" este instanta a clasei Show

instance Show Dow where
  show Mon = "Luni"
  show Tue = "Marti"
  show Wed = "Miercuri"
  show Thu = "Joi"
  show Fri = "Vineri"
  show Sat = "Sambata"
  show Sun = "Duminica"

-- Atentie! Clasele de tipuri in Haskell
--                !=
--          clasele din C++
--  Clasele din Haskell seamana cu
--     interfetele/clasele abstracte in Java/C++
--  Clasele din Haskell seamana cu conceptele in C++

--data Nat = Zero | Succ Nat deriving (Show, Ord)
data Nat = Succ Nat | Zero deriving Show

trei = Succ (Succ (Succ Zero))
doi = Succ (Succ Zero)

instance Eq Nat where
  (==) Zero Zero = True
  (==) Zero (Succ _) = False
  (==) (Succ _) Zero = False
  (==) (Succ x) (Succ y) = x == y
  (/=) Zero Zero = False
  (/=) Zero (Succ _) = True
  (/=) (Succ _) Zero = True
  (/=) (Succ x) (Succ y) = x /= y

instance Ord Nat where
  (<=) Zero Zero = True
  (<=) Zero (Succ _) = True
  (<=) (Succ _) Zero = False
  (<=) (Succ x) (Succ y) = x <= y

class MyEq a where
  egal :: a -> a -> Bool
  egal x y = not (notegal x y)
  notegal :: a -> a -> Bool
  notegal x y = not (egal x y)

instance MyEq Nat where 
  egal Zero Zero = True
  egal Zero (Succ _) = False
  egal (Succ _) Zero = False
  egal (Succ x) (Succ y) = x == y
  notegal Zero Zero = False
  notegal Zero (Succ _) = True
  notegal (Succ _) Zero = True
  notegal (Succ x) (Succ y) = x /= y

data Intreg = Pos Nat | Neg Nat deriving Show

iminustrei = Neg trei
itrei = Pos trei

instance Eq Intreg where
  (==) (Pos x) (Pos y) = x == y
  (==) (Neg x) (Neg y) = x == y
  (==) (Pos x) (Neg y) = (x == Zero) && (y == Zero)
  (==) (Neg x) (Pos y) = (x == Zero) && (y == Zero)

-- Bounded = clasa tipurilor marginite

-- paranteza (

-- Tipul de date unit (notat "()") are exact o valoare, si anume valoarea "()"
-- similar cu tipul "void" in C(++)

asdf :: () -> Int
asdf () = 3

asdf' :: Int -> ()
asdf' _ = ()

asdf'' :: Int -> ()
asdf'' 3 = ()
asdf'' x = asdf'' x

-- paranteza )

-- clasa Enum

-- clasa Num

-- exemplu: un tip de date pentru numere complexe
--          as vrea sa fie instanta a clasei Num

-- Functori

impartire :: Int -> Int -> Maybe Int
impartire _ 0 = Nothing
impartire x y = Just (div x y)

cmmdc :: Int -> Int -> Maybe Int
cmmdc 0 0 = Nothing
cmmdc x y = if x == y then Just x
            else if x < y then cmmdc x (y - x)
            else cmmdc (x - y) y
            
-- * este un "kind"

-- kind-urile sunt pentru tipuri
-- ce sunt tipurile pentru valori

--  argumentul lui Maybe
--         vvv
-- Maybe :: * -> *
--              ^^^
--            intoarce un tip de kind *
-- Maybe este un constructor de tipuri
-- care primeste ca argument un tip de kind *

-- Maybe :: * -> *
-- Int :: *
-- Maybe Int :: *

data Reason = DivByZero | NotDefined deriving Show

impartire' :: Int -> Int -> Either Reason Int
impartire' _ 0 = Left DivByZero
impartire' x y = Right (div x y)

cmmdc' :: Int -> Int -> Either Reason Int
cmmdc' 0 0 = Left NotDefined
cmmdc' x y = if x == y then Right x
             else if x < y then cmmdc' x (y - x)
             else cmmdc' (x - y) y

-- cand as vrea ca un tip de date pe care il definesc sa faca parte din clasa Functor?

data Arb a = Empty | Nod a (Arb a) (Arb a)
           deriving (Show, Eq)

--                  vvvvvvvvvvvvvvvvvvvv
--fmap' :: (a -> b) -> ((Arb a) -> (Arb b))
--fmap' f Empty = Empty
--fmap' f (Nod info left right) =
--       Nod (f info) (fmap' f left) (fmap' f right)

instance Functor Arb where
  --fmap :: (a -> b) -> ((Arb a) -> (Arb b))
  fmap f Empty = Empty
  fmap f (Nod info left right) =
         Nod (f info) (fmap f left) (fmap f right)
  