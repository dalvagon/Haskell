-- Tipuri de Date Algebrice

-- ---> Algebraic Data Type (ADT)

-- a nu se confunda cu Abstract Data Type (ADT)

-- Cele mai simple tipuri algebrice de date sunt tipurile enumerative

{-

enum DOW 
{
  MONDAY,
  TUESDAY,
  // ...
};

-}


data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show
--         ^^^
--      obligatoriu incep cu majuscula
--      se numesc constructori (mult diferiti fata de constructorii din C++/Java)

{-
Sintaxa:
- cuvantul cheie "data" (definesc un tip de date)
- Day = numele tipului pe care il definesc (obligatoriu incepe cu majuscula)
  [numele de functii/variabile incep obligatoriu cu litera mica]
- egal
- valorile, separate prin "|"
-}

nextDay :: Day -> Day
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

{-
ghci este un REPL (read-evaluate-print loop)
-}



-- Constructorii pentru ADTuri pot sa aiba parametri

impartire :: Integer -> Integer -> Integer
impartire x y = if y == 0 then 0 else div x y
--                            ^^^
--                  problematic fiindca pot confunda cu un cat al impartirii valid


data Rezultat = Invalid | Valid Integer deriving Show
--                        ^^^^^^^^^^^^^
--           constructorul "Valid" are un parametru, iar parametrul este un Integer
-- valorile posibile de tip Rezultat sunt:
-- Invalid :: Rezultat
-- "Valid" nu este o valoare
-- Valid 0 :: Rezultat
-- Valid 1 :: Rezultat
-- Valid 7 :: Rezultat
-- Valid (-3) :: Rezultat
-- ...

impartire' :: Integer -> Integer -> Rezultat
impartire' x y = if y == 0 then Invalid else Valid (div x y)

aux :: Rezultat -> Bool
aux Invalid = False
aux (Valid _) = True

impartireOk :: Integer -> Integer -> Bool
impartireOk x y = aux (impartire' x y)


-- Asemanari si deosebiri intre functii si constructori:
-- + asemanare: sintaxa de apel
-- - deosebire 0: constructorii incep cu majuscula
-- - deseobire 1: constructorul nu face niciun calcul (doar eticheteaza parametrii)
-- - deosebire 2: constructorii pot fi folositi in pattern matching


-- functiile nu pot fi folosite in pattern matching
-- ce s-ar intampla daca as putea folosi functii in pattern matching?

-- fimaginara :: Integer -> Bool
-- fimaginara (n * m) | n /= 1 && m /= 1 = False
--            ^^^^^^^
--      nu e permis sa apeleze functii aici
-- fimaginara _                          = True


data Pereche = P Integer Integer deriving Show

prima :: Pereche -> Integer
prima (P x y) = x

adoua :: Pereche -> Integer
adoua (P x y) = y

-- de ce se cheama "tipuri algebrice"?


-- sunt obtinute prin aplicarea a doua operatii peste tipuri:
-- 1. produsul (product)
--       am un constructor cu mai multi parametri
-- 2. suma (reuniunea) disjuncta (disjoint union).
--       am mai multi constructori


-- Tipuri Recursive

-- un constructor poate lua ca parametru tipul de date care este in curs de definire
-- exemplul canonic: lista

data Lista = Vida | Cons Integer Lista deriving Show

nrelem :: Lista -> Int
nrelem Vida = 0
nrelem (Cons hd tl) = 1 + nrelem tl

suma :: Lista -> Integer
suma Vida = 0
suma (Cons hd tl) = hd + suma tl

-- primele n numere naturale strict pozitive
primelen :: Integer -> Lista
primelen 0 = Vida
primelen n = Cons n (primelen (n - 1))

-- exercitiu bun: lista in ordine (hint: functie auxiliara)



-- Tipuri de date parametrice
data ListaBool = VidaBool | ConsBool Bool ListaBool deriving Show

nrelemBool :: ListaBool -> Int
nrelemBool VidaBool = 0
nrelemBool (ConsBool hd tl) = 1 + nrelemBool tl




-- "a" este o "variabila de tip"
-- "a" tine locul unui tip de date
-- exemplu: a = Integer sau a = Bool sau a = ... orice tip ...
data List a = Emp | Con a (List a) deriving (Show, Eq)

-- instantieri: List Integer -- lista de numere intregi
--              List Bool -- lista de Bool-uri

countElem :: List a -> Int
countElem Emp = 0
countElem (Con hd tl) = 1 + countElem tl

sumLista :: List Integer -> Integer
sumLista Emp = 0
sumLista (Con hd tl) = hd + sumLista tl

-- reverse :: List a -> List a
-- exercitiu

-- convert :: List a -> [a]
-- convert' :: [a] -> List a

-- expresii aritmetice

data Exp = Const Float
         | Suma Exp Exp
         | Produs Exp Exp
         | Var String
           deriving (Show, Eq)

eval :: Exp -> Float
eval (Const x) = x
eval (Suma e1 e2) = (eval e1) + (eval e2)
eval (Produs e1 e2) = (eval e1) * (eval e2)
eval (Var v) = 0 -- gasesc o metoda mai inteligenta de a evalua variabilele

simpl :: Exp -> Exp
simpl (Const x) = Const x
simpl (Var v) = Var v
simpl (Suma e1 e2) = let e1' = simpl e1 in
                     let e2' = simpl e2 in
                     case (e1', e2') of
                       (Const x, Const y) -> Const (x + y)
                       (Const 0, _)       -> e2'
                       (_, Const 0)       -> e1'
                       _                  -> Suma e1' e2'
simpl (Produs e1 e2) = Produs (simpl e1) (simpl e2)

-- deriv :: Exp -> String -> Exp
-- ... deriv (Produs (Var 'x') (Var 'x')) 'x' = Produs (Const 2) (Var 'x')

-- expression problem
-- cum am defini o expresie aritmetica intr-un limbaj OO
-- versus
--                                     intr-un limbaj cu ADTuri
-- si care sunt avantajele si dezavantajele in fiecare caz

{-

class Exp // f. probabil: clasa abstracta
{
  virtual float eval() = 0;
  virtual Exp simpl() = 0;
};

class Const : public Exp
{
  float x;
  float eval()
  {
    return x;
  }
  Exp simpl()
  {
     return *this;
  }
};

class Var : public Exp
{
  string v;
  float eval()
  {
    return 0;
  }
  Exp simpl()
  {
     return *this;
  }
};

class Suma : public Exp
{
  Exp e1;
  Exp e2;       
  float eval()
  {
    return e1.eval() + e2.eval();
  }
  Exp simpl()
  {
     Exp e1p = e1.simpl();
     Exp e2p = e2.simpl();
     // ...     <- putin problematic de scris fiindca nu am pattern matching in C++
     return // ... ;
  }
};

class Produs : public Exp
{
  Exp e1;
  Exp e2;
  float eval()
  {
    return e1.eval() * e2.eval();
  }
  Exp simpl()
  {
     Exp e1p = e1.simpl();
     Exp e2p = e2.simpl();
     return // ...;
  }
};

-}

-- Avantaj C++: adaugarea unui caz nou
--              -- doar scriu o clasa noua care mosteneste Exp
--                 (nu am de modificat nimic altceva)
--              -- in Haskell: am de modificat toate functiile care prelucreaza Exp-uri

-- Avantaj Haskell: adaug o functie noua care prelucreaza Exp-uri
--              -- doar scriu functia in Haskell
--              -- in C++: adaug cate o implementare a functiei in fiecare dintre clase


-- Arbori Binari de Cautare
-- Arbori AVL
