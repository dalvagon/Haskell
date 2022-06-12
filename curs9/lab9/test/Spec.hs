import Nat
import Conversion
import Test.QuickCheck

prop1 :: Int -> Property
prop1 x = (x >= 0) ==> (natToInt (intToNat x) == x)

prop2 :: Nat -> Bool
prop2 x = intToNat (natToInt x) == x


main :: IO ()
main = do quickCheck prop1
          quickCheck prop2

{-
+++ OK, passed 100 tests; 101 discarded.
+++ OK, passed 100 tests.
-}


