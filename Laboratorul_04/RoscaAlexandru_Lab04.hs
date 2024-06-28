-- Nume:  Rosca Alexandru-David
-- Grupa: A4
-- Anul:  II
-- Laborator: IV

-- //////////////////////////////////////////////////////////////

-- !Exercitiul 1.1

addThree :: Int -> Int -> Int -> Int
addThree x y z  = x + y + z

-- //////////////////////////////////////////////////////////////

-- !Exercitiul 2.1

sumOf :: (Int -> Int) -> Int -> Int -> Int
sumOf f lowerBound upperBound | lowerBound == upperBound = f lowerBound
sumOf f lowerBound upperBound = f lowerBound + sumOf f (lowerBound + 1) upperBound

-- ghci> sumOf (+ 2) 1 2
-- 7
-- ghci> sumOf (+ 0) 1 10
-- 55
-- ghci> sumOf (* 0) 1 10
-- 0
-- ghci> sumOf (* 2) 1 10
-- 110

-- !Exercitiul 2.2

function1 :: Int -> Int
function1 x = x + 10 

function2 :: Int -> Int
function2 x = x * 10

combineFunc :: (a -> a) -> (a -> a) -> (a -> a)
combineFunc f1 f2 = f1 . f2

combinedFunctions = combineFunc function1 function2

-- ghci> combinedFunctions 10
-- 110
-- ghci> combinedFunctions 100
-- 1010

-- !Exercitiul 2.3

combineArray :: [a -> a] -> (a -> a)
combineArray [] = id
combineArray (x:xs) = x . combineArray xs

combinedFunctions1 = combineArray [function1, function1, function1]

-- ghci> combinedFunctions1 10
-- 40

-- !Exercitiul 2.4

sumOfList :: [Int] -> Int
sumOfList [] = 0
sumOfList (x:xs) = x + sumOfList xs

-- ghci> sumOfList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
-- 55

-- !Exercitiul 2.5

applyFunction :: (a -> b) -> [a] -> [b]
applyFunction f [] = []
applyFunction f (x:xs) = f x : applyFunction f xs

-- ghci> applyFunction (+ 2) [1, 2, 3, 4, 5]
-- [3,4,5,6,7]


-- !Exercitiul 2.6

returnTrue :: (a -> Bool) -> [a] -> [a]
returnTrue f [] = []
returnTrue f (x:xs) | f x = x : returnTrue f xs
returnTrue f (x:xs) = returnTrue f xs

-- ghci> returnTrue (&& True) [True, False, False, True]
-- [True,True]
-- ghci> returnTrue (> 5) [1, 2, 3, 4, 5, 6]
-- [6]

-- !Exercitiul 2.7

foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f acc [] = acc
foldR f acc (x:xs) = f x (foldR f acc xs)  

-- ghci> foldR (+) 0 [4,3,2,1]
-- 10
-- ghci> foldR (*) 1 [4,3,2,1]
-- 24

foldL :: (b -> a -> b) -> b -> [a] -> b
foldL f acc [] = acc
foldL f acc (x:xs) = let acc' = acc `f` x
                     in foldl f acc' xs

-- ghci> foldl (+) 0 [1,2,3,4]
-- 10
-- ghci> foldl (+) 1 [1,2,3,4]
-- 11
-- ghci> foldl (*) 1 [1,2,3,4]
-- 24

-- foldL [1,2,3,4]
-- (1 + foldL [2,3,4])
-- ((1 + 2) + foldL [3,4])
-- (((1 + 2) + 3) + foldL [4]) 
-- ((((1 + 2) + 3) + 4) + foldL [])
-- (((1 + 2) + 3) + 4)

-- !Exercitiul 2.8

data Arb = Frunza
         | Nod Int Arb Arb 
         deriving (Show, Eq)

applyPreOrder :: Arb -> (Int -> b) -> [b]
applyPreOrder Frunza _ = []
applyPreOrder (Nod nodeValue arbS arbD) f = f nodeValue : applyPreOrder arbS f ++ applyPreOrder arbD f

applyInOrder :: Arb -> (Int -> b) -> [b]
applyInOrder Frunza _ = []
applyInOrder (Nod nodeValue arbS arbD) f = applyInOrder arbS f ++ f nodeValue : applyInOrder arbD f

applyPostOrder :: Arb -> (Int -> b) -> [b]
applyPostOrder Frunza _ = []
applyPostOrder (Nod nodeValue arbS arbD) f = applyPostOrder arbS f ++ applyPostOrder arbD f ++ [f nodeValue]

-- ghci> applyInOrder (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 8) (+ 2)
-- [4,5,6,7,8,9,10]

-- !Exercitiul 2.9

chooseOption :: String -> Int
chooseOption "preordine" = 1
chooseOption "inordine" = 2
chooseOption "postordine" = 3
chooseOption "orice-ordine" = 4
chooseOption _ = 4

treeWalk :: (String -> Int) -> String -> Arb -> [Int]
treeWalk _ _ Frunza = []
treeWalk f typeOfParsing (Nod nodeValue arbS arbD) | f typeOfParsing == 1 = nodeValue : treeWalk f typeOfParsing arbS ++ treeWalk f typeOfParsing arbD 
treeWalk f typeOfParsing (Nod nodeValue arbS arbD) | f typeOfParsing == 2 = treeWalk f typeOfParsing arbS ++ nodeValue : treeWalk f typeOfParsing arbD 
treeWalk f typeOfParsing (Nod nodeValue arbS arbD) = treeWalk f typeOfParsing arbS ++ treeWalk f typeOfParsing arbD  ++ [nodeValue]

-- ghci> treeWalk chooseOption "preordine" (Nod 5 (Nod 3 Frunza Frunza) Frunza)                                          
-- [5,3]
-- ghci> treeWalk chooseOption "orice-ordine" (Nod 5 (Nod 3  Frunza Frunza) Frunza)
-- [3,5]

-- //////////////////////////////////////////////////////////////

-- !Exercitiul 3.1

isBigger :: Ord a => a -> a -> Bool
isBigger x y | x > y = True
isBigger x y = False

sortArray :: Eq a => [a] -> (a -> a -> Bool) -> [a]
sortArray [] _ = []
sortArray [x] _ = [x]
sortArray (x:xs) f = let smallest = findSmallest (x:xs) f 
                     in smallest : sortArray (removeSmallest (x:xs) smallest) f

findSmallest :: [a] -> (a -> a -> Bool) -> a
findSmallest [x] _ = x
findSmallest (x:xs) f = let y = findSmallest xs f
                        in if f x y 
                                then y
                           else 
                                x

removeSmallest :: Eq a => [a] -> a -> [a]
removeSmallest [] _ = []
removeSmallest (x:xs) value | value /= x = x : removeSmallest xs value
removeSmallest (x:xs) value = removeSmallest xs value

-- ghci> sortArray [5,4,3,6,1] isBigger
-- [1,3,4,5,6]

-- !Exercitiul 3.2

data Either a = Left String
              | Right a
              deriving Show

-- !Exercitiul 3.3

data Tree a = Leaf
            | Node a (Tree a) (Tree a) 
             deriving (Show, Eq, Ord)

-- !Exercitiul 3.4

cautareSecventiala :: Eq a => [a] -> a -> Bool
cautareSecventiala [] _ = False
cautareSecventiala (x:xs) value = let result = cautareSecventiala xs value 
                                  in ((value == x) || (result || False))

cautareSecventialaFold :: Eq a => [a] -> a -> Bool
cautareSecventialaFold xs x = foldr (\ y acc -> acc || x == y) False xs 

-- ghci> cautareSecventiala [1,2,3,4,5,6] 9
-- False
-- ghci> cautareSecventiala [1,2,3,4,5,6] 2
-- True

cautareBinara :: Ord a => [a] -> a -> Bool
cautareBinara [] _ = False
cautareBinara [x] value = x == value
cautareBinara xs value | getLast xs == value = True
cautareBinara xs value | getLast xs > value = cautareBinara (firstHalf xs) value 
cautareBinara xs value = cautareBinara (lastHalf xs) value

firstHalf :: [a] -> [a]
firstHalf xs = take (length xs `div` 2) xs

lastHalf :: [a] -> [a]
lastHalf xs = drop (length xs `div` 2) xs

getLast :: [a] -> a
getLast [x] = x
getLast (_:xs) = getLast xs

-- ghci> cautareBinara [1,2,3,4,5,6] 3
-- True
-- ghci> cautareBinara [1,2,3,4,5,6] 10
-- False

-- //////////////////////////////////////////////////////////////

-- !Exercitiul 4.1

fromend :: [a] -> Int -> Maybe a
fromend xs x     | length xs < x = Nothing 
fromend (x:xs) value | length (x:xs) - 1 == value = Just x
fromend (x:xs) value = fromend xs value

-- ghci> fromend [1,2,3,4,5] 1
-- Just 4
-- ghci> fromend [1,2,3,4,5] 0
-- Just 5
-- ghci> fromend [1,2,3,4,5] 4
-- Just 1
-- ghci> fromend [1,2,3,4,5] 5
-- Nothing

-- !Exercitiul 4.2 / 4.3

convolute :: [a] -> [a] -> [(a, a)]
convolute xs xs1 = convolute' xs (Main.reverse xs1 )

convolute' :: [a] -> [a] -> [(a, a)]
convolute' [] [] = []
convolute' (x:xs) (x1:xs1) = (x, x1) : convolute' xs xs1


reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = Main.reverse xs ++ [x]

-- ghci> convolute [1,2,3] [1,2,3]
-- [(1,3),(2,2),(3,1)]
-- ghci> convolute [1, 7, 5] [1, 2, 3]
-- [(1,3),(7,2),(5,1)]

-----------------------------------------------------