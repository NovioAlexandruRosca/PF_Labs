module Main where

import Nat (add)
import NatToInt (int_to_nat, nat_to_int)

main :: IO()
main = do
    putStrLn "Introdu doua numere intregi:"
    num1 <- readLn :: IO Int
    num2 <- readLn :: IO Int
    let nat1 = int_to_nat num1
        nat2 = int_to_nat num2
        sum_nat = add nat1 nat2
        sum_int = nat_to_int sum_nat
    putStrLn $ "Suma format natural: " ++ show sum_nat
    putStrLn $ "Suma format întreg: " ++ show sum_int

