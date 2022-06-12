f x = 3 + x

-- Polimorfism in Haskell

-- aceeasi functie functioneaza cu mai multe tipuri de date

-- 1. polimorfismul parametric
-- 2. polimorfismul ad-hoc

-- 1. Polimorfismul Parametric
-- ===========================
-- trateaza in acelasi mod toate tipurile

data Lista a = Vida | Cons a (Lista a) deriving (Show, Eq)

lungime :: Lista a -> Int
lungime Vida = 0
lungime (Cons _ tail) = 1 + lungime tail

-- "lungime" este o functie polimorfica (poate functiona la fel de bine si cu Lista Bool, Lista Int, ...)

-- intuitiv: functia nu "se uita in" interiorul tipului "a"


-- 2. Polimorfismul Ad-hoc
-- implementat prin intermediul claselor de tip

qs :: Ord a => [a] -> [a]
qs [] = []
qs (h:t) = qs (filter (<=h) t) ++ [h] ++ qs (filter (>h) t)

-- de fiecare data cand am un tip constrans --> functia este polimorfica
-- adhoc in tipul respectiv

-- ex1 :: Ord a => [a] -> [b] -> [(a, b)]

-- De ce credeti ca e interesant polimorfismul parametric?

-- Exemplul 1.
-- ex2 :: a -> b
-- implementarea lui ex2 e secreta
-- singura implementare posibila: ex2 x = ex2 x

-- Exemplul 2.
-- sunt doua implementari posibila
ex3a :: a -> a
ex3a x = x -- prima implementare

ex3b :: a -> a
ex3b = ex3b -- a doua  implementare

-- Exemplul 3.
ex3 :: [a] -> a
ex3 [] = ex3 []
ex3 (h:t) = h

-- intoarce capul listei
-- bucleaza la infinit
-- intoarce al 2-lea element din lista
-- intoarce al 3-lea element din lista
-- in functie de lungimea listei, sa am unul dintre comportamentele de mai sus

-- functia ex3 sigur nu este definita pe lista [] sau,
-- daca este definita, sigur ex3 [] are bucla infinita

-- Exemplul 4.
v :: a
-- singura implementare pentru v:
v = v

-- Teoreme Gratuite (orice functie polimorfica parametrica are teoreme grauite)

-- Teorema 1.
-- Daca
--   f :: [a] -> a si
--   ex3 [1, 2, 3] == 2,
-- atunci
--   ex3 [True, False, True] == False.



-- Evaluare Lenesa

-- evaluare, in general, se refera la modalitatea
-- in care ajung la o valoare pornind de la o expresie.

-- Liceu:

-- Pentru a executa instructiunea de atribuire
-- "x = expresie"
-- se evalueaza intai "expresie" si apoi rezultatul este stocat
-- in variabila x.

-- Pentru apelul unei functii "f(exp1, ..., expn)", se evalueaza intai
-- fiecare expresie (exp1, apoi exp2, ..., apoi expn) si apoi se apeleaza
-- functia propriu-zis.

-- Limbajele in care sunt adevarate afirmatiile de mai sus
-- au o strategie de evaluare care se numeste "evaluare eager".
--                                                      ^^^^^
--                                                    nerabdator


-- Haskell e la polul opus: nicio expresie nu este evaluata decat daca
-- rezultatul este neaparat necesar.

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

a :: Int
a = 2

-- let x = expresie in expresie2 

b :: Int
b = let x = fib 100 in
    let y = 13 in
    if y > 10 then 2 else x

g :: Int -> Int -> Int
g x y = if x > 2 then y else 10

-- intr-un limbaj eager:
-- g 1 (fib 100) =
-- g 1 (fib 99 + fib 98) =
-- g 1 ((fib 98 + fib 97) + fib 98) =
-- ...

-- Ecuatii built-in:
-- if True then a else b = a
-- if False then a else b = b

-- intr-un limbaj lazy:
-- g 1 (fib 100) =
-- if 1 > 2 then (fib 100) else 10 =
-- if False then (fib 100) else 10 =
-- 10.

ite :: Bool -> a -> a -> a
ite True x y = x
ite False x y = y

{-
int ite(bool b, int x1, int x2)
{
  if (b) {
    return x1;
  } else {
    return x2;
  }
}
--> ar evalua atat x1 cat si x2
-}


-- Cum este implementata evaluarea lazy in Haskell?

-- fiecare variabila nu contine neaparat valoarea variabilei,
--                   ci un pointer catre o bucata de cod care,
--                                         ^^^^^^^^^^^^^
--                                      se numesc thunk-uri
--                   daca este apelata, produce valoarea pe care o vreau.


-- exista mai multe strategii de tip lazy

-- o potentiala problema a evaluarii lazy
double :: Int -> (Int, Int)
double x = (x, x)


-- double (fib 30) =
-- (fib 30, fib 30) =
-- ...
-- (832040, fib 30) =
-- ...
-- (832040, 832040)

-- Haskell foloseste o strategie numita call-by-need care asigura ca
-- expresiile duplicat sunt evaluate cel mult o data


-- Despre strategii de evaluare --> lambda-calcul (spre sfarsitul semestrului)

-- Ce pot face interesant cu evaluarea lazy?

-- pot defini structuri de date aparent infinite

lista :: [Int]
-- lista contina lista tuturor numerelor naturale

listaAux :: Int -> [Int]
listaAux x = x : listaAux (x + 1)

lista = listaAux 0

-- lista :: [Int] nu tine minte lista propriu-zis, ci este un pointer
-- catre o bucata de cod care produce lista (daca este nevoie)

pare = map (*2) lista

-- (!!) :: [a] -> Int -> a
-- (!!) (h:t) 0 = h
-- (!!) (h:t) x = (!!) t (x-1)
-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (h:t) = (f h : map f t)

-- (map (*2) lista) !! 0 =
-- (map (*2) (listaAux 0)) !! 0 =
-- (map (*2) (0:(listaAux (0 + 1)))) !! 0 =
--       ^^  ^  ^^^^^^^^^^^^^^^^^^
--        f  h   t
-- ((*2) 0 : map (*2) (listaAux (0 + 1))) !! 0 =
--  ^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
--    h           t
-- (*2) 0 =
-- 0.

-- Cand am evaluare lenesa: e mai greu sa rationez
--                          despre timpul de executie al unui algoritm.
