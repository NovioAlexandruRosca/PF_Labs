module NatToInt (int_to_nat, nat_to_int) where

import Nat (Nat(Zero, Succ)) 

int_to_nat :: Int -> Nat
int_to_nat 0 = Zero
int_to_nat n = Succ (int_to_nat (n - 1))

nat_to_int :: Nat -> Int
nat_to_int Zero = 0
nat_to_int (Succ n) = 1 + nat_to_int n 
