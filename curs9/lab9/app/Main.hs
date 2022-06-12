module Main where

import Nat
import Conversion

main :: IO ()
main = do putStrLn "First Number:"
          x <- getLine
          let natX = (fromInteger (read x) :: Nat)
          print $ show natX
          putStrLn "Second Number:"
          y <- getLine
          let natY = (fromInteger (read y) :: Nat)
          print $ show natY
          let sum = natX + natY
          print $ "The sum is " ++ show (fromEnum sum)

{-
 stack exec lab9-exe     
First Number:
10
"Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))"
Second Number:
2
"Succ (Succ Zero)"
"The sum is 12"


-- testele in test/Spec.hs
-}
