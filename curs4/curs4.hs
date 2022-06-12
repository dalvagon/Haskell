-- f :: Integer -> Integer -> Integer
-- f x y = x + y


-- adder :: Integer -> (Integer -> Integer)
-- adder x = \y -> x + y

-- f3 :: Integer -> Integer 
-- f3 = adder 3

-- mistery :: (Integer  -> Integer) -> Integer 
-- mistery f = f 7

-- add2 :: Int -> Int
-- add2 x = x + 2

twice f x = f (f x)
adder :: Int -> (Int -> Int)
adder y = \x -> x + y
add2 = adder 

reduce :: [a] -> b -> (b -> a -> b) -> b
reduce [] init _ = init
reduce (x:xs) init f = reduce xs (f init x) f

