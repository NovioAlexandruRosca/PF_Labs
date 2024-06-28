-- factorial :: Int -> Int
-- factorial 0 = 1
-- factorial x = x * factorial (x - 1)

---------------------------------------------------------------------

-- factorial :: Int -> Int
-- factorial x = factorial' x 1

-- factorial' :: Int -> Int -> Int
-- factorial' 0 acc = acc
-- factorial' x acc = factorial' (x - 1) (acc * x)

---------------------------------------------------------------------

-- executeNTimes :: IO() -> Int -> IO()
-- executeNTimes f 0 = return ()
-- executeNTimes f x = do
--                     f
--                     executeNTimes f (x - 1)

---------------------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]

-- + :: Num a :: a -> a -> a

-- map (+) [1, 2, 3] -> :t Num a :: [a -> a]

---------------------------------------------------------------------

-- filtreaza :: [Int] -> [Int]
-- filtreaza = map (\x -> x `div` 2) . filter even 

-- sau

-- filtreaza :: [Int] -> [Int]
-- filtreaza x = (map (\x -> x `div` 2) . filter even ) x

---------------------------------------------------------------------

-- data Exp = Var String
--          | Const Int
--          | Pow Exp Exp
--          | Plus Exp Exp
--          | Mul Exp Exp
--          | Par Exp
--          deriving (Show)

-- --  Mul (Par (Plus (Pow (Const 2) (Var "x")) (Const 7))) (Var "y")

---------------------------------------------------------------------

-- data ListaNevida = Val Int
--                  | Const Int ListaNevida
--                  deriving (Show)

-- lungime :: ListaNevida -> Int
-- lungime (Val _)  = 1
-- lungime (Const _ l) = 1 + lungime l

---------------------------------------------------------------------

-- half :: Int -> Maybe Int
-- half x | even x = Just $ div x 2
-- half _          = Nothing

-- -- (==>) :: (Maybe Int) -> (Int -> Maybe Int) -> Maybe Int
-- -- Nothing ==> _ = Nothing
-- -- Just x ==> f = f x

-- (==>) :: (Maybe Int) -> (Int -> Maybe Int) -> Maybe Int
-- (==>) = (>>=)


---------------------------------------------------------------------


-- sorting :: [Int] -> [Int]
-- sorting [] = []
-- sorting (x:xs) = sorting lesser ++ [x] ++ sorting greater
--         where 
--             lesser = [y | y <- xs , y<= x]
--             greater = [y | y <- xs, y > x]


-- mergeSort :: (Ord a) => [a] -> [a]
-- mergeSort [] = []
-- mergeSort [x] = [x]
-- mergeSort xs = merge (mergeSort left) (mergeSort right)
--     where
--         (left, right) = splitAt (length xs `div` 2) xs

-- merge :: (Ord a) => [a] -> [a] -> [a]
-- merge [] ys = ys
-- merge xs [] = xs
-- merge (x:xs) (y:ys)
--     | x <= y    = x : merge xs (y:ys)
--     | otherwise = y : merge (x:xs) ys

-- sumList :: [Int] -> Int
-- sumList = foldl (+) 0 . map (+ 2) . filter (\x -> x `mod` 2 == 0)

-- data Boolean = Var String
--              | Or Boolean Boolean
--              | And Boolean Boolean
--              | Not Boolean
--              | Par Boolean
--              deriving (Show) 

-- x && ((y || z) && (!x && z))
-- And (Var "x") (Par (And (Or (Var "y") (Var "z")) (And (Not (Var "x")) (Var "z"))))

-- f1 :: Int -> Int
-- f1 x = x + 2

-- f2 :: Int -> Int
-- f2 x = x + 3

-- f3 :: Int -> [(Int -> Int)] -> Int
-- f3 n [] = n
-- f3 n (x:xs) = x (f3 n xs)



-- countEven :: [Int] -> Int
-- countEven [] = 0
-- countEven (x:xs) = if even x then 1 + countEven xs
--                         else countEven xs

-- sau

-- countEven :: [Int] -> Int
-- countEven [] = 0
-- countEven (x:xs) | even x = 1 + countEven xs
--                  | otherwise = countEven xs

-- countEven :: [Int] -> Int
-- countEven l = countEven' l 0


-- countEven' :: [Int] -> Int -> Int
-- countEven' [] acc = acc
-- countEven' (x:xs) acc = if even x then countEven' xs (acc + 1)
--                                   else countEven' xs acc

-- countdown :: Int -> [IO ()]
-- countdown x | x <= 0 = []
-- countdown n          = (putStrLn (show n)) : countdown (n - 1)

-- runActions :: [IO a] -> IO () 
-- runActions []     = return ()
-- runActions (a:as) = do 
--                         a 
--                         (runActions as)


-- sumEven :: [Int] -> Int 
-- sumEven l = foldl (+) 0 (filter even l)

-- data ListaNevida a = Last a
--                    | Elem a (ListaNevida a)
--                    deriving (Show)

-- -- lung :: ListaNevida Int -> Int -> Int
-- -- lung (Last _) acc = acc + 1
-- -- lung (Elem _ f) acc = lung (f) (acc + 1) 

-- lung :: ListaNevida Int -> Int
-- lung (Last _) = 1
-- lung (Elem _ f) = 1 + lung f


-- half :: Int -> Maybe Int
-- half x | even x = Just $ div x 2
-- half _ = Nothing


-- (==>) :: (Maybe Int) -> (Int -> Maybe Int) -> Maybe Int
-- (==>) = (>>=)

----------------------------------------------

-- string2Integer :: String -> Integer
-- string2Integer "" = 0
-- string2Integer x = help x 0 (10 ^ (len x - 1))

-- len :: String -> Integer
-- len [] = 0
-- len (x:xs) = 1 + len xs

-- help :: String -> Integer -> Integer -> Integer
-- help [] acc _ = acc
-- help (x:xs) acc pow = help xs (char2Integer x * pow + acc) (pow `div` 10)

-- char2Integer :: Char -> Integer
-- char2Integer c = toInteger (fromEnum c) - 48
    
------------------------------------------------

-- countCapitals :: String -> Int
-- countCapitals [] = 0
-- countCapitals (x:xs) | x <= 'Z' && x >= 'A' = 1 + countCapitals xs
-- countCapitals (x:xs) | otherwise = countCapitals xs 

------------------------------------------------

-- type Model = String
-- type Marca = String
-- type An = String
-- type Tonaj = Integer

-- data Vehicle = Car Marca Model An
--              | Ship Marca An
--              | Bicycle Marca Model
--              | Truck Marca Model Tonaj
--              deriving (Show)

-- vehicleBrand :: Vehicle -> String
-- vehicleBrand (Car marca _ _) = marca
-- vehicleBrand (Ship marca _) = marca
-- vehicleBrand (Bicycle marca _) = marca
-- vehicleBrand (Truck marca _ _) = marca

------------------------------------------------

-- data Exp = Var String
--          | Const Int
--          | Plus Exp Exp
--          | Mul Exp Exp
--          | Mins Exp Exp
--          | Div Exp Exp
--          | Par Exp
--          deriving (Show)

------------------------------------------------

-- countDigits :: String -> Int
-- countDigits x = length (filter (\x -> x >= '0' && x <= '9') x)

------------------------------------------------

-- appFOverList :: [a] -> ( a -> [a] ) -> [a]
-- appFOverList [] f = []
-- appFOverList (x:xs) f = f x ++ (appFOverList xs f) 

-- fGen :: [Int -> Int]
-- fGen = (map (+) [1, 2, 3])

-- (=*=>) :: [a -> b] -> [a] -> [b]
-- [] =*=> l = []
-- (x:xs) =*=> l = (map x l) ++ (xs =*=> l) 

-- reverseIO :: [IO()] -> [IO()]
-- printIO [] = return ()
-- printIO (x:xs) = do
--                   x
--                   printIO xs

-- suma :: Int -> Int
-- suma n | n == 0 = 0
-- suma n | otherwise = n + suma(n - 1)

-- map (+2) [1,2,3,4]
-- filter (\x -> even x) [1,2,3,4]
-- foldl (+) 0 [1,2,3,4]

-- (+) :: a -> a -> a
-- (+2) :: a -> a

-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Boolean) -> [a] -> [a]
-- foldl :: (a -> a -> b) -> b -> [a] -> b
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ($) :: (a -> b) -> a -> b

-- data Exp = Var String
--          | Const Int
--          | Plus Exp Exp
--          | Mul Exp Exp
--          | Mins Exp Exp
--          | Div Exp Exp
--          | Par Exp


-- instance Show Exp where
--   show :: Exp -> String
--   show (Var x) = x 
--   show (Const x) = show x
--   show (Plus x y) = show x ++ " + " ++ show y
--   show (Mul x y) =  show x ++ " * " ++ show y
--   show (Mins x y) = show x ++ " - " ++ show y
--   show (Div x y) =  show x ++ " / " ++ show y
--   show (Par x) =    "(" ++ show x ++ ")"


increment_2 :: [Int] -> [Int]
increment_2 x = ls ++ rn
                where
                ls = map (+2) (filter odd x)
                rn = filter even x


qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) = qs ls ++ [x] ++ qs gr
            where 
            ls = [y | y <- xs , y <= x]
            gr = [y | y <- xs, y > x]


-- data Maybe a = Justt a
--              | Nothingg

-- l -> ((x || y) && z)

data Exp = Var String
         | Or Exp Exp
         | And Exp Exp
         | Imp Exp Exp
         | Dimp Exp Exp
         | Par Exp

instance Show Exp where
  show :: Exp -> String
  show (Var x) = x
  show (Or x y) = show x ++ " || " ++ show y   
  show (And x y) = show x ++ " && " ++ show y   
  show (Imp x y) = show x ++ " => " ++ show y   
  show (Dimp x y) = show x ++ " <=> " ++ show y   
  show (Par x) = "(" ++ show x ++ ")"


-- (+++) :: String -> Int -> String
-- x +++ y = x ++ " " ++ show y

-- foldl (+++) "0" [1,2,3,4]
-- foldl :: (b -> a -> b) -> b -> [a] -> b

--         "0 1 2 3 4"


string2Integer :: String -> Integer
string2Integer [] = 0
string2Integer (x:xs) = (10 ^ length xs) * char2Integer x + string2Integer xs


-- string2Integer :: String -> Integer
-- string2Integer [] = 0
-- string2Integer xs = aux xs 0

aux :: String -> Integer -> Integer
aux [] acc = acc
aux (x:xs) acc = aux xs (acc * 10 + (char2Integer x))

char2Integer :: Char -> Integer
char2Integer c = toInteger (fromEnum c) - 48


countCapitals :: String -> Int
countCapitals [] = 0
countCapitals (x:xs) | x >= 'A' && x <= 'Z' = 1 + countCapitals xs
countCapitals (x:xs)  = countCapitals xs


data Vehicle = Car String String Int
             | Ship String Int
             | Bicycle String String
             | Truck String String Int
             deriving (Show)

vehicleBrand :: Vehicle -> String
vehicleBrand (Car marca _ _) = marca
vehicleBrand (Ship marca _) = marca
vehicleBrand (Bicycle marca _) = marca
vehicleBrand (Truck marca _ _) = marca


lol :: Maybe Int -> Maybe Int
lol (Just a) = Just a
lol (Nothing) = Nothing    





import Prelude hiding (Left, Right)

data Arb = Nil | Node Int Arb Arb deriving (Show, Eq)

t1 :: Arb
t1 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 Nil Nil)

f :: Arb -> Int -> Arb
f (Node x (Node y (Node o a1 a2) a3) z) v = (Node x (Node y (Node v a1 a2) a3) z)

data Dir = L | R deriving (Show, Eq)

type Poz = [Dir]

at :: Arb -> Poz -> Arb
at a [] = a
at (Node _ a1 _) (L : p) = at a1 p
at (Node _ _ a2) (R : p) = at a2 p

change :: Arb -> Poz -> Int -> Arb
change Nil [] _ = error "Nu exista valoarea veche"
change Nil _ _ = error "Nu exista pozitia"
change (Node o a1 a2) [] v = Node v a1 a2
change (Node x a1 a2) (L : p) v = (Node x (change a1 p v) a2)
change (Node x a1 a2) (R : p) v = (Node x a1 (change a2 p v))

p = [L, L]

data Crumb = Left Int Arb | Right Int Arb deriving (Show, Eq)

type Trail = [ Crumb ]

goLeft :: (Arb, Trail) -> (Arb, Trail)
goLeft (Nil, _) = error "Cannot go left in leaf"
goLeft (Node x a1 a2, t) = (a1, (Left x a2) : t)

goRight :: (Arb, Trail) -> (Arb, Trail)
goRight (Nil, _) = error "Cannot go left in leaf"
goRight (Node x a1 a2, t) = (a2, (Right x a1) : t)

goUp :: (Arb, Trail) -> (Arb, Trail)
goUp (a, []) = error "Cannot go up in root"
goUp (a, ((Left x a2) : t)) = ((Node x a a2), t)
goUp (a, ((Right x a1) : t)) = ((Node x a1 a), t)

change' :: (Arb, Trail) -> Int -> (Arb, Trail)
change' (Nil, _) _ = error "Cannot change information in leaf"
change' (Node x a1 a2, t) v = (Node v a1 a2, t)

