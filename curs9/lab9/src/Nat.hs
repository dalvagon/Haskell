module Nat where

import Test.QuickCheck

data Nat = Zero | Succ Nat deriving (Show, Eq)

instance Num Nat where
  (+) Zero y = y
  (+) (Succ x) y = Succ ((+) x y)
  (*) Zero y = Zero
  (*) (Succ x) y = (+) x ((*) x y)
  abs x = x
  signum _ = 1
  fromInteger 0 = Zero
  fromInteger x = Succ (fromInteger (x - 1))
  negate _ = Zero

instance Arbitrary Nat where
  arbitrary = oneof (map (return . fromInteger) [0..100])