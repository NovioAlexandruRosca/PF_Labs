-- Nume:  Rosca Alexandru-David
-- Grupa: A4
-- Anul:  II
-- Laborator: V

-- //////////////////////////////////////////////////////////////

-- !Exercitiul 0.3 (Am adaugat aici diferite functii de care o sa am nevoie la exercitiile urmatoare)

-- data Byte = Zero | Unu
        --   deriving (Show, Eq, Ord) 

data Nat = Binary [Bool]
           deriving Show


helperBinaryToNumber :: Nat -> Int
helperBinaryToNumber (Binary xs) = binaryToNumber (Binary xs) (2 ^ (lengthOfBinary (Binary xs) - 1))

binaryToNumber :: Nat -> Int -> Int
binaryToNumber (Binary []) _ = 0
binaryToNumber (Binary (x:xs)) power | x = power + binaryToNumber (Binary xs) (power `div` 2)
binaryToNumber (Binary (x:xs)) power = binaryToNumber (Binary xs) (power `div` 2)

-- ghci> binaryToNumber (Binary [True, True, True]) 4
-- 7
-- ghci> binaryToNumber (Binary [True, False, True]) 4
-- 5

helperNumberToBinary :: Int -> Nat
helperNumberToBinary value = numberToBinary value []

numberToBinary :: Int -> [Bool] -> Nat
numberToBinary 0 xs = Binary (reverse xs) 
numberToBinary value xs = numberToBinary (value `div` 2) (xs ++ (if value `mod` 2 == 1 then [True] else [False]))

-- ghci> numberToBinary 6 []
-- Binary [True,True,False]
-- ghci> numberToBinary 5 []
-- Binary [True,False,True]

lengthOfBinary :: Nat -> Int
lengthOfBinary (Binary []) = 0
lengthOfBinary (Binary (x:xs)) = 1 + lengthOfBinary (Binary xs)

-- ghci> lengthOfBinary (Binary [])
-- 0
-- ghci> lengthOfBinary (Binary [True, True, False])
-- 3

-- !Exercitiul 0.4

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (Binary xs) == (Binary ys) = xs == ys

-- ghci> Binary [True, True, True, False] == Binary [True, True, True, False]
-- True
-- ghci> Binary [True, True, True, False] == Binary [True, True, True, True]
-- False


instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  compare (Binary xs) (Binary ys) = compare (binaryToNumber (Binary xs) (2 ^ (lengthOfBinary (Binary xs) - 1))) (binaryToNumber (Binary ys) (2 ^ (lengthOfBinary (Binary ys) - 1)))

-- ghci> Binary [True, True, True] > Binary [False, False, False]
-- True
-- ghci> Binary [True, True, True] < Binary [False, False, False]
-- False

instance Enum Nat where
  succ :: Nat -> Nat 
  succ (Binary xs) = numberToBinary (binaryToNumber (Binary xs) (2 ^ (lengthOfBinary (Binary xs) - 1)) + 1) []
  pred :: Nat -> Nat 
  pred (Binary xs) = numberToBinary (binaryToNumber (Binary xs) (2 ^ (lengthOfBinary (Binary xs) - 1)) - 1) []
  toEnum :: Int -> Nat
  toEnum = helperNumberToBinary
  fromEnum :: Nat -> Int
  fromEnum = helperBinaryToNumber

-- succ (Binary [False, True, True])
-- Binary [True,False,False]

instance Real Nat where
  toRational :: Nat -> Rational
  toRational (Binary xs) = toRational (binaryToNumber (Binary xs) (2 ^ (lengthOfBinary (Binary xs) - 1)))

-- ghci> toRational (Binary [False, True, True])
-- 3 % 1

instance Integral Nat where
  toInteger :: Nat -> Integer 
  toInteger (Binary xs) = toInteger (helperBinaryToNumber (Binary xs))
  quotRem :: Nat -> Nat -> (Nat, Nat)
  quotRem (Binary xs) (Binary ys) =
    (numberToBinary ( binaryToNumber (Binary xs) (2 ^ (lengthOfBinary (Binary xs) - 1)) `div`
                       binaryToNumber (Binary ys) (2 ^ (lengthOfBinary (Binary ys) - 1))) [],
     numberToBinary ( binaryToNumber (Binary xs) (2 ^ (lengthOfBinary (Binary xs) - 1)) `mod`
                       binaryToNumber (Binary ys) (2 ^ (lengthOfBinary (Binary ys) - 1))) [])
    
-- ghci> Binary [False, True, True] `div` Binary [False, True, True]
-- Binary [True]

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (Binary xs) + (Binary ys) = numberToBinary ((+) (binaryToNumber (Binary xs) (2 ^ (lengthOfBinary (Binary xs) - 1)))
                       (binaryToNumber (Binary ys) (2 ^ (lengthOfBinary (Binary ys) - 1)))) []
  (*) :: Nat -> Nat -> Nat
  (Binary xs) * (Binary ys) = numberToBinary ((*) (binaryToNumber (Binary xs) (2 ^ (lengthOfBinary (Binary xs) - 1)))
                       (binaryToNumber (Binary ys) (2 ^ (lengthOfBinary (Binary ys) - 1)))) []

  signum :: Nat -> Nat
  signum (Binary []) = 0
  signum _ = 1
  
  fromInteger :: Integer -> Nat
  fromInteger value | value Prelude.< 0 = error "Cannot represent negative numbers"
  fromInteger value = helperNumberToBinary (fromIntegral value)

  abs :: Nat -> Nat
  abs xs = xs 

  negate :: Nat -> Nat
  negate xs = xs

-- ghci> Binary [True, False, True] + Binary [False, True, False]
-- Binary [True,True,True]
-- ghci> Binary [True, True, True] + Binary [False, True, False]
-- Binary [True,False,False,True]

-- !Exercitiul 0.5

data Complex a = Complex { realPart :: a, imaginaryPart :: a }

instance (Num a, Ord a, Floating a) => Num (Complex a) where

    (+) :: Num a => Complex a -> Complex a -> Complex a
    (Complex a b) + (Complex c d) = Complex (a + c) (b + d)
    
    (-) :: Num a => Complex a -> Complex a -> Complex a
    (Complex a b) - (Complex c d) = Complex (a - c) (b - d)

    (*) :: Num a => Complex a -> Complex a -> Complex a
    (Complex a b) * (Complex c d) = Complex (a * c - b * d) (a * d + b * c)

    negate :: Num a => Complex a -> Complex a
    negate (Complex a b) = Complex (-a) (-b)

    abs :: (Num a, Floating a) => Complex a -> Complex a
    abs (Complex a b) = Complex (sqrt (a^2 + b^2)) 0

    signum :: Num a => Complex a -> Complex a
    signum (Complex a b) = Complex (signum a) (signum b)

    fromInteger :: Num a => Integer -> Complex a
    fromInteger value = Complex (fromInteger value) 0

-- ghci> abs (Complex 5 5)
-- 7.0710678118654755
-- ghci> Complex 3 2 * Complex 5 (-3)
-- 21 + i
-- ghci> negate (Complex 5 3)
-- -5 - 3i
-- ghci> fromInteger 1
-- 1
-- ghci> negate (Complex 5.5 3)
-- -5.5 - 3.0i

instance (Show a, Num a, Eq a, Ord a) => Show (Complex a) where
    show :: (Show a, Num a, Eq a) => Complex a -> String
    show (Complex a b) | b == 0    = show a
    show (Complex a b) | a == 0    = show b ++ "i"
    show (Complex a b) = show a ++ (if b Prelude.< 0 then " - " else " + ") ++ (if b /= 1 && b /= -1 then show (abs b) else "") ++ "i"


class Eq a => MyOrd a where
    (<) :: a -> a -> Bool
    x < y = not (x Main.>= y)
    
    (<=) :: a -> a -> Bool
    x <= y = x Main.< y || x == y

    (>) :: a -> a -> Bool
    x > y = not (x Main.<= y)
    
    (>=) :: a -> a -> Bool
    x >= y = not (x Main.< y)

instance MyOrd Int where
    (<), (<=), (>), (>=) :: Int -> Int -> Bool
    x < y = x Main.< y
    x <= y = x Main.<= y
    x > y = x Main.> y
    x >= y = x Main.>= y

instance MyOrd a => MyOrd [a] where
    (<), (<=), (>), (>=) :: [a] -> [a] -> Bool
    
    [] < [] = False
    [] < _ = True
    _ < [] = False
    (x:xs) < (y:ys) | x == y = xs Main.< ys
    (x:xs) < (y:ys) = x Main.< y
    xs > ys = ys Main.< xs

    xs <= ys = xs Main.< ys || xs == ys

    xs >= ys = ys Main.<= xs

sort :: MyOrd a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = merge (sort firstHalf) (sort secondHalf)
  where
    (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs
    
merge :: MyOrd a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x Main.<= y = x : merge xs (y:ys)
merge (x:xs) (y:ys) = y : merge (x:xs) ys

-- ghci> sort ([4,3,1,2] :: [Int])
-- [1,2,3,4]

-- !Exercitiul 0.7

data Natt = Zero 
         | Succ Natt
        --  deriving(Show, Eq, Ord)

-- ghci> Succ (Succ Zero) Prelude.< Succ Zero
-- False
-- ghci> Succ (Succ Zero) Prelude.> Succ Zero
-- True
-- ghci> Succ (Succ Zero) Prelude.== Succ Zero
-- False

-- !Exercitiul 0.8

instance Show Natt where
    show :: Natt -> String
    show Zero = "o"
    show (Succ xs) = "s " ++ show xs

-- ghci> Succ (Succ (Succ Zero))
-- s s s o 

-- !Exercitiul 0.9  

instance Ord Natt where
    (<=) :: Natt -> Natt -> Bool
    Zero <= Zero = True
    Zero <= Succ _ = True
    Succ _ <= Zero =  False
    Succ x <= Succ y = x Prelude.<= y
    (<) :: Natt -> Natt -> Bool
    Zero < Zero = False
    Zero < Succ _ = True
    Succ _ < Zero = False
    Succ x < Succ y = x Prelude.< y

-- ghci> Succ (Succ Zero) Prelude.> Succ Zero
-- True
-- ghci> Succ (Succ Zero) Prelude.< Succ Zero
-- False
-- ghci> Zero Prelude.< Zero
-- False

-- !Exercitiul 0.10

data Arb = Frunza
         | Nod Integer Arb Arb 
         deriving (Eq)

instance Show Arb where
    show :: Arb -> String
    show Frunza = "()"
    show (Nod x left right) = "(" ++ show x ++ show left ++ show right ++ ")"

-- ghci> (Nod 2 (Nod 3 Frunza Frunza) (Nod 4 Frunza Frunza))
-- (2(3()())(4()()))

-- !Exercitiul 0.11

data Arbb a = Leaf
           | Node a (Arbb a) (Arbb a)

instance (Show a) => Show (Arbb a) where
    show :: Arbb a -> String
    show Leaf = "()"
    show (Node x left right) = "(" ++ show x ++ show left ++ show right ++ ")"

-- ghci> (Node 5 Leaf Leaf)
-- (5()())
-- ghci> (Node 5.5 Leaf Leaf)
-- (5.5()())

-- "a" trebuie sa faca parte din clasa Show deoarece s ar putea ca a sa nu poata sa fie converit la un string ca sa fie afisat
-- aka nu implementeaza Show(toate tipurile basic implementeaza show dar daca aveam un tip facut de noi si l foloseam nu mai mergea)

-- !Exercitiul 0.12

instance Eq Natt where
    (==) :: Natt -> Natt -> Bool
    Zero == Zero = True
    Succ _ == Zero = False
    Zero == Succ _ = False
    Succ x == Succ y = x == y 

-- ghci> (Succ Zero) == Zero
-- False
-- ghci> Zero == Zero
-- True
-- ghci> Succ Zero == Succ Zero
-- True

-- !Exercitiul 0.13

instance (Eq a) => Eq (Arbb a) where
    (==) :: Arbb a -> Arbb a -> Bool
    Leaf == Leaf = True
    (Node x xleft xright) == Leaf = False
    Leaf == (Node y yleft yright) = False
    (Node x xleft xright) == (Node y yleft yright) = x == y && (xleft == yleft) && (xright == yright)

-- ghci> (Node 5 Leaf Leaf) == (Node 5 Leaf Leaf)
-- True
-- ghci> (Node 5 Leaf Leaf) == (Node 5 Leaf (Node 5 Leaf Leaf))
-- False


-- !Exercitiul 0.14

class Pretty a where
    prettyPrint :: a -> String


instance Pretty Natt where
    prettyPrint Zero = "0"
    prettyPrint (Succ n) = "1 + " ++ prettyPrint n


instance (Pretty a,Show a) => Pretty (Arbb a) where
    prettyPrint Leaf = "()"
    prettyPrint (Node x left right) = "Node (" ++ show x ++ prettyPrint left ++ ") (" ++ prettyPrint right ++ ")"

-- ghci> prettyPrint (Succ (Succ (Succ Zero)))
-- "1 + 1 + 1 + 0"

class MyNum a where
    toInt :: a -> Int

instance MyNum Natt where
    toInt :: Natt -> Int
    toInt Zero = 0
    toInt (Succ n) = 1 + toInt n

-- ghci> toInt (Succ (Succ (Succ Zero)))
-- 3

