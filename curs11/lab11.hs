-- Leahu Vlad A6

type Id = String

data Term = Var Id | Lambda Id Term | App Term Term deriving (Show, Eq)

-- EX 1
x :: Id
x = "x"
y :: Id
y = "y"
z :: Id
z = "z"
v :: Id
v = "v"
u :: Id
u = "u"
w :: Id
w = "w"

ids :: [Id]
ids = [x, y, z, u, v, w];

term :: Term
term = Lambda x (Lambda y (Var x))


-- EX 2
subst :: Id -> Term -> Term -> Term
subst id term (Var id') | id == id' = term
                        | otherwise = Var id'
subst id term (Lambda id' term') | id == id' = Lambda id' term'
                                 | otherwise = Lambda id' (subst id term term')
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
{-
*Main> subst x (Var y) (Var x)
Var "y"

*Main> subst y (Var z) (Var x)
Var "x"

*Main> subst y (Var z) (App (Var x) (Var y))
App (Var "x") (Var "z")

*Main> subst y (Var z) (App (Var y) (Var x))
App (Var "z") (Var "x")

*Main> subst x (Lambda z (Var z)) (Lambda x (App (Var y) (Var x)))
Lambda "x" (App (Var "y") (Var "x"))

*Main> subst x (Lambda z (Var z)) (Lambda y (App (Var y) (Var x)))
Lambda "y" (App (Var "y") (Lambda "z" (Var "z")))

*Main> subst y (Var x) (Lambda x (Var y))
Lambda "x" (Var "x")
-}

-- EX 3
remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (x : xs) | id == x = remove id xs
                   | otherwise = x : remove id xs
{-
*Main> remove x [x, y, z, x, w, u, x, v]
["y","z","w","u","v"]
-}

-- EX 4
free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)
{-
*Main> free (Lambda x (Lambda y (Lambda x (App (App (Var x) (Var y)) (Var z)))))
["z"]
-}

-- EX 5
vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) =  id : vars term
{-
*Main> vars (Lambda x (Lambda y (Lambda x (App (App (Var x) (Var y)) (Var z)))))
["x","y","x","x","y","z"]
-}


-- EX 6
fresh' :: [Id] -> Int -> Id
fresh' ids index = if  ("x" ++ show index) `elem` ids then
                            fresh' ids (index + 1)
                        else
                            "x" ++ show index

fresh :: [Id] -> Id
fresh ids = fresh' ids 0
{-
*Main> fresh ids
"x0"
*Main> fresh $ [fresh ids] ++ ids
"x1"
-}

-- EX 7
casubst' :: Id -> Term -> Term -> [Id] -> Term
casubst' id term (Var id') _ | id == id' = term
                             | otherwise = Var id'
casubst' id term (Lambda id' term') avoid | id == id' = Lambda id' term'
                                          | id' `elem` free term = 
                                              let id'' = fresh avoid in 
                                                  Lambda id'' (casubst' id term (subst id' (Var id'') term') (id'' : avoid))
                                          | otherwise = Lambda id' (casubst' id term term' avoid)
casubst' id term (App term1 term2) avoid = App (casubst' id term term1 avoid) (casubst' id term  term2 avoid)

casubst :: Id -> Term -> Term -> Term
casubst id term term' = casubst' id term term' (vars term')
{-
*Main> casubst x (Var y) (Var x) 
Var "y"

*Main> casubst y (Var z) (Var x) 
Var "x"

*Main> casubst y (Var z) (App (Var x) (Var y))      
App (Var "x") (Var "z")

*Main> casubst y (Var z) 
App (Var "z") (Var "x")

*Main> casubst x (Lambda z (Var z)) (Lambda x (App (Var y) (Var x)))
Lambda "x" (App (Var "y") (Var "x"))

*Main> casubst x (Lambda z (Var z)) (Lambda y (App (Var y) (Var x)))
Lambda "y" (App (Var "y") (Lambda "z" (Var "z")))

*Main> casubst y (Var x) (Lambda x (Var y))
Lambda "x0" (Var "x")
-}

-- EX 8
reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing 
reduce1' (Lambda id term) avoid = case reduce1' term avoid of
  Nothing -> Nothing
  Just term' -> Just (Lambda id term')
reduce1' (App (Lambda id term) term') avoid = 
    Just (casubst' id term' term avoid)
reduce1' (App term1 term2) avoid = case reduce1' term1 avoid of  
  Nothing -> case reduce1' term2 avoid of
    Nothing -> Nothing
    Just term2' -> Just (App term1 term2')
  Just term1' -> Just (App term1' term2)

reduce1 :: Term -> Maybe Term
reduce1 term = reduce1' term (vars term)

term1 :: Term
term1 = Lambda x (Var x)
term2 :: Term
term2 = App term1 term1
term3 :: Term
term3 = Lambda y (Lambda x term2)
term4 :: Term
term4 = App term3 term
{-
*Main> reduce1 term1
Nothing

*Main> reduce1 term2
Just (Lambda "x" (Var "x"))

*Main> reduce1 term3
Just (Lambda "y" (Lambda "x" (Lambda "x" (Var "x"))))

*Main> reduce1 term4
Just (Lambda "x" (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))))
-}


reduce :: Term -> Term
reduce term = case reduce1 term of  
  Nothing -> term
  Just term' -> reduce term'
{-
*Main> reduce (App (Lambda x (Var x)) (App (Lambda z (Var z)) (Var y)))
Var "y"
-}

-- EX 10
{-
*Main> reduce (App (Lambda x (App (Var x) (Var x))) (Lambda x (App (Var x) (Var x))))
-- ruleaza la infinit
-}

reduceFor :: Int -> Term -> Term
reduceFor 0 term = term
reduceFor n term = case reduce1 term of  
  Nothing -> term
  Just term' -> reduceFor (n - 1) term'


-- EX 11
tTRUE = Lambda x (Lambda y (Var x))
tFALSE = Lambda x (Lambda y (Var y))
tAND = Lambda x (Lambda y (App (App (Var x) (Var y)) (Var x)))
tOR = Lambda x (Lambda y (App (App (Var x) (Var x)) (Var y)))
tNOT = Lambda x (App (App (Var x) tFALSE) tTRUE)
{-
*Main> reduce $ App (App tOR tFALSE) (App tNOT tTRUE)            
Lambda "x" (Lambda "y" (Var "y"))

*Main> reduce $ App tNOT (App tAND (App tFALSE (App tOR (App tFALSE tTRUE ))))
Lambda "x" (Lambda "y" (Var "x"))
-}

-- EX 12
f :: Id
f = "f"
n :: Id
n = "n"
m :: Id
m = "m"

tZERO = Lambda f (Lambda x (Var x))
tSUCC = Lambda n (Lambda f (Lambda x (App (App (Var n) (Var f)) (App (Var f) (Var x)))))

tONE = reduce $ App tSUCC tZERO
tTWO = reduce $ App tSUCC tONE
tTHREE = reduce $ App tSUCC tTWO
tPLUS = Lambda n (Lambda m (Lambda f (Lambda x (App (App (Var m) (Var f)) (App (App (Var n) (Var f)) (Var x))))))

tFIVE = reduce $ App (App tPLUS tTWO) tTHREE
{-
*Main> tFIVE 
Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))))   
-}

tMULT = Lambda n (Lambda m (Lambda f (Lambda x (App (App (Var m) (App (Var n) (Var f))) (Var x)))))

tMUL :: Term
tMUL = reduce $ App (App tMULT tTWO) tTHREE
{-
*Main> tMUL
Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))))
-}