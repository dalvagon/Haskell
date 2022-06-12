-- Leahu Vlad A6

type Id = String

data Term = Var Id | Lambda Id Term | App Term Term deriving (Show, Eq)

subst :: Id -> Term -> Term -> Term
subst id term (Var id') | id == id' = term
                        | otherwise = Var id'
subst id term (Lambda id' term') | id == id' = Lambda id' term'
                                 | otherwise = Lambda id' (subst id term term')
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (x : xs) | id == x = remove id xs
                   | otherwise = x : remove id xs

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)

vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) =  id : vars term

fresh' :: [Id] -> Int -> Id
fresh' ids index = if  ("x" ++ show index) `elem` ids then
                            fresh' ids (index + 1)
                        else
                            "x" ++ show index

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

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

reduce :: Term -> Term
reduce term = case reduce1 term of  
  Nothing -> term
  Just term' -> reduce term'

reduceFor :: Int -> Term -> Term
reduceFor 0 term = term
reduceFor n term = case reduce1 term of  
  Nothing -> term
  Just term' -> reduceFor (n - 1) term'

term1 :: Term
term1 = App (Lambda "x1" (Var "x1")) (App (Lambda "x2" (Var "x2")) (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))))

term2 :: Term
term2 = App (Lambda "x1" (Lambda "x2" (Var "x2"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))

-- ?? - Strategia de evaluare este Normal Order
{-
*Main> reduce term1
Lambda "z" (Var "z")

*Main> reduce term2
Lambda "x2" (Var "x2")
-}

-- EX 2

cbn1 :: Term -> Maybe Term
cbn1 (Lambda id term) = Nothing
cbn1 term = reduce1 term

cbn :: Term -> Term
cbn term = case cbn1 term of
  Nothing -> term
  Just term' -> cbn term'
{-
*Main> cbn term1
Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))

*Main> cbn term2
Lambda "x2" (Var "x2")

Un avantaj al strategiei call-by-name este faptul ca parametrii care nu sunt
folositi in calcul in corpul functiei nu sunt evaluati

*Main> Just (App (Lambda "x1" (App (Var "x1") (Var "x1"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))) >>= cbn1 >>= cbn1 >>= cbn1 >>= cbn1         
Just (Lambda "y" (Var "y"))

-- Calculul asociat strategiei call-by-name se realizeaza in 4 pasi
-}


-- EX 3

strategy1' :: Term -> [Id] -> [Term]
strategy1' (Var _) _ = []
strategy1' (App (Lambda id term) term') avoid = [casubst' id term' term avoid] ++ 
    let all = strategy1' term avoid in
    let all' = strategy1' term' avoid in
    [ App (Lambda id successorTerm) term' | successorTerm <- all ] ++
    [ App (Lambda id term) successorTerm' | successorTerm' <- all']
strategy1' (App term1 term2) avoid =
    let all1 = strategy1' term1 avoid in
    let all2 = strategy1' term2 avoid in
    [ App sterm1 term2 | sterm1 <- all1 ] ++
    [ App term1 sterm2 | sterm2 <- all2 ]
strategy1' (Lambda id term) avoid =
    let all = strategy1' term avoid in
    [ Lambda id sterm | sterm <- all ]

strategy1 :: Term -> [Term]
strategy1 term = strategy1' term (vars term)

strategy :: Term -> [Term]
strategy term = let all = strategy1 term in case all of
    [] -> [term]
    _ -> concat (map strategy all)

{-
Strategia folosita este Full Beta Reduction - se aplica Beta reducerea in toate locurile posibile

*Main> strategy term1
[Lambda "z" (Var "z"),Lambda "z" (Var "z"),Lambda "z" (Var "z"),Lambda "z" (Var "z"),Lambda "z" (Var "z"),Lambda "z" (Var "z")]

*Main> strategy term2
[Lambda "x2" (Var "x2"),Lambda "x2" (Var "x2")]
-}

-- EX 4

cbv1 :: Term -> Maybe Term
cbv1 (Lambda id term) = Nothing
cbv1 (App (Lambda x term) term') = case cbv1 term' of 
  Nothing -> reduce1 (App (Lambda x term) term')
  Just term'' -> Just (App (Lambda x term) term'')
cbv1 term = reduce1 term

cbv :: Term -> Term
cbv term = case cbv1 term of
  Nothing -> term
  Just term' -> cbv term'

{-
*Main> cbv term1
Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))

*Main> cbv term2
Lambda "x2" (Var "x2")

*Main> cbv (App (Lambda "x1" (App (Var "x1") (Var "x1"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))))
Lambda "y" (Var "y")
-}


-- EX 5

term51 :: Term
term51 = App (Lambda "x1" (Lambda "x2" (Var "x2"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))

term52 :: Term
term52 = App (Lambda "x1" (App (Var "x1") (Var "x1"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y")))

{-
*Main> Just term51 >>= cbv1 >>= cbv1         
Just (Lambda "x2" (Var "x2"))

*Main> Just term51 >>= cbn1
Just (Lambda "x2" (Var "x2"))


*Main> Just term52 >>= cbv1 >>= cbv1 >>= cbv1
Just (Lambda "y" (Var "y"))

*Main> Just term52 >>= cbn1 >>= cbn1 >>= cbn1 >>= cbn1
Just (Lambda "y" (Var "y"))
-- 
-}

-- EX 6

app1 :: Term -> Maybe Term
app1 (App (Lambda x term) term') = case app1 term' of 
  Nothing -> reduce1 (App (Lambda x term) term')
  Just term'' -> Just (App (Lambda x term) term'')
app1 term = reduce1 term

app :: Term -> Term
app term = case cbv1 term of
  Nothing -> term
  Just term' -> app term'