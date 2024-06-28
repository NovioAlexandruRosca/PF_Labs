module Nat (Nat(Zero, Succ), zero, succ', add, mul) where

data Nat = Zero | Succ Nat
    deriving Show

instance Eq Nat where
  Zero == Zero = True
  Succ m == Succ n = m == n
  _ == _ = False

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

succ' :: Nat -> Nat
succ' n = Succ n

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Succ n) m = add m (mul n m)

zero :: Nat
zero = Zero