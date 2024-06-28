module Test where

import Test.QuickCheck
import Nat
import NatToInt

instance Arbitrary Nat where
    arbitrary = sized genNat
      where
        genNat 0 = return Zero
        genNat n = map Succ (genNat (n - 1))


p1 :: Int -> Bool
p1 x = nat_to_int (int_to_nat x) == x

p2 :: Nat -> Bool
p2 x = int_to_nat (nat_to_int x) == x

main :: IO ()
main = do
    quickCheck p1
    quickCheck p2
