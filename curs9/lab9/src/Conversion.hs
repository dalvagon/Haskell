module Conversion where

import Nat

instance Enum Nat where
    toEnum = fromInteger . fromIntegral
    fromEnum Zero = 0
    fromEnum (Succ x) = 1 + fromEnum x

intToNat :: Int -> Nat
intToNat = toEnum

natToInt :: Nat -> Int
natToInt = fromEnum
