-- Rosca Alexandru-David
-- A4
-- An 2

-- 2.2

-- cmd > 2
-- cmd > (+) 2 3
-- cmd > ((+) ((*) 3 5) 2)
-- cmd > ((*) ((+) 2 3) 5)
-- cmd > (/) 3 5
-- cmd > (*) 45345345346536 54425523454534333
-- cmd > (/) 3 0
-- cmd > True
-- cmd > False
-- cmd > (&&) True False
-- cmd > (||) True False
-- cmd > not True
-- cmd > (<=) 2 3
-- cmd > (not ((<=) 2 3))
-- cmd > ((||) ((<=) 2 3) True)
-- cmd > (==) "aaa" "aba"
-- cmd > (==) "aba" "aba"
-- cmd > (++) "aaa" "aba"

-- 2.3

-- :t True
-- :t False
-- :t True && False
-- :t True && (2 <= 4)

-- 2.4

-- :t "aaa"

-- 2.5

-- :t 2
-- :t 2 + 3
-- :t (+)

-- Creeaza o lista
-- createList n = if n <= 0 
--         then []
--         else n:createList (n - 1)


createList :: Int -> [Int]
createList n | n <= 0 = []
createList n = n : createList (n - 1)

-- ghci> createList 10
-- [10,9,8,7,6,5,4,3,2,1]

-- Functie care calculeaza maximul a 3 numere
mymax :: Int -> Int -> Int -> Int
mymax x y z =
    if x <= y then
        if y <= z then z else y
    else
        if x <= z then z else x

-- ghci> mymax 50 (-1) 2
-- 50

-- Fib 
fibo x | x <= 2 = 1 
fibo x = fibo (x - 1) + fibo (x - 2)

-- ghci> fibo 15
-- 610

cmmdc :: Integral t => t -> t -> t
cmmdc x y = 
    if y == 0
        then x
    else cmmdc y (x `mod` y)

-- ghci> cmmdc 105 20
-- 5

main :: IO()
main = do
    putStrLn "Hello World"

-- primele n elemente dintr o lista
-- elementele de dupa primele n elemente

-- firstN :: [Int] => Int

firstN (x:xs) n | n <= 0 = []
firstN (x:xs) n = x:firstN xs (n - 1)

-- ghci> firstN [1,2,3] 2
-- [1,2]

-- Trebuie sa adaug ceva ca sa mearga pentru n >= [].length
