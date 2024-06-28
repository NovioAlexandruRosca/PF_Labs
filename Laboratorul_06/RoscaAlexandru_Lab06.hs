-- Nume:  Rosca Alexandru-David
-- Grupa: A4
-- Anul:  II
-- Laborator: VI

-- //////////////////////////////////////////////////////////////

import System.CPUTime

-- !Exercitiul 1

minimArray :: [Int] -> Int
minimArray xs = minArray xs maxBound

minArray :: (Ord a) => [a] -> a -> a
minArray [] smallest = smallest
minArray (x:xs) smallest | x < smallest = minArray xs x
minArray (x:xs) smallest = minArray xs smallest  -- Nu e terminat dar am vrut sa fac un sort in care returnez
                                                 -- primul element si apoi sa scot elementul respectiv din lista si sa apelez iar functia

sortArray :: (Ord a) => [a] -> [a]
sortArray [] = []
sortArray (x:xs) = let leftSort = sortArray [a | a <- xs, a <= x]
                       rightSort = sortArray [a | a <- xs, a > x]
                          in  leftSort ++ [x] ++ rightSort

-- ghci> minArray [5,4,3,2,1] maxBound::Int
-- 1

-- !Exercitiul 2

cpuTime :: IO a -> IO Double
cpuTime f = do
    startingTime <- getCPUTime
    runningSession <- f
    endingTime <- getCPUTime
    let time = fromIntegral (endingTime - startingTime) / (10^12) :: Double
    return time

showResults :: (Ord a, Show a) => (a -> b) -> a -> IO ()
showResults f xs = do
        time <- cpuTime (return $! f xs )
        putStrLn $ "Time elapsed: " ++ show time ++ " seconds."

showSortResults :: (Ord a, Show a) => (c -> a -> b) -> a -> c -> IO ()
showSortResults f xs sortingAlg = do
        time <- cpuTime (return $! f sortingAlg xs )
        putStrLn $ "Time elapsed: " ++ show time ++ " seconds."

hugeList :: [Int]
hugeList = replicate 5000 0
    
-- ghci> showResults minimArray hugeList
-- CPU time elapsed: 0.0 seconds.
-- ghci> showResults sortArray hugeList
-- CPU time elapsed: 8.25 seconds.

-- !Exercitiul 3

firstLista :: ([Int] -> [Int]) -> [Int] -> Int
firstLista f xs = head (f xs)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
    where
        insert :: (Ord a) => a -> [a] -> [a]
        insert x [] = [x]
        insert x (y:ys) | x <= y = x:y:ys
        insert x (y:ys)          = y : insert x ys

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort leftArray) (mergeSort rightArray)
    where
        (leftArray, rightArray) = splitAt (length xs `div` 2) xs
        merge :: (Ord a) => [a] -> [a] -> [a]
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
        merge (x:xs) (y:ys)          = y : merge (x:xs) ys

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (hd:tl) = quickSort (filter (<=hd) tl) ++ [hd] ++ quickSort (filter (>hd) tl)

-- ghci> showSortResults firstLista hugeList quickSort
-- CPU time elapsed: 2.46875 seconds.
-- ghci> showSortResults firstLista hugeList mergeSort
-- CPU time elapsed: 1.5625e-2 seconds.
-- ghci> showSortResults firstLista hugeList insertionSort
-- CPU time elapsed: 0.0 seconds.
-- ghci> showResults quickSort hugeList
-- CPU time elapsed: 4.125 seconds.
-- ghci> showResults mergeSort hugeList
-- CPU time elapsed: 3.125e-2 seconds.
-- ghci> showResults insertionSort hugeList
-- CPU time elapsed: 0.0 seconds.

lastLista :: ([Int] -> [Int]) -> [Int] -> Int
lastLista f xs = last (f xs)

-- ghci> showSortResults lastLista hugeList quickSort
-- CPU time elapsed: 4.435 seconds. // probabil pentru ca am laptop-ul pe power saving mode
-- ghci> showSortResults lastLista hugeList mergeSort
-- CPU time elapsed: 4.350e-2 seconds.
-- ghci> showSortResults lastLista hugeList insertionSort
-- CPU time elapsed: 0.0 seconds.

-- !Exercitiul 4

maximArray :: [Int] -> Int
maximArray xs = maxArray xs minBound

maxArray :: (Ord a) => [a] -> a -> a
maxArray [] biggest = biggest
maxArray (x:xs) biggest | x > biggest = maxArray xs x
maxArray (x:xs) biggest = maxArray xs biggest 

-- !Exercitiul 5

fibo :: [Integer]
fibo = 0 : 1 : nextFibo 0 1

nextFibo :: Integer -> Integer -> [Integer]
nextFibo x y = z : nextFibo y z
    where z = x + y

-- ghci> take 10 fibo
-- [0,1,1,2,3,5,8,13,21,34]
-- ghci> take 100 fibo
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733,1134903170,1836311903,2971215073,4807526976,7778742049,12586269025,20365011074,32951280099,53316291173,86267571272,139583862445,225851433717,365435296162,591286729879,956722026041,1548008755920,2504730781961,4052739537881,6557470319842,10610209857723,17167680177565,27777890035288,44945570212853,72723460248141,117669030460994,190392490709135,308061521170129,498454011879264,806515533049393,1304969544928657,2111485077978050,3416454622906707,5527939700884757,8944394323791464,14472334024676221,23416728348467685,37889062373143906,61305790721611591,99194853094755497,160500643816367088,259695496911122585,420196140727489673,679891637638612258,1100087778366101931,1779979416004714189,2880067194370816120,4660046610375530309,7540113804746346429,12200160415121876738,19740274219868223167,31940434634990099905,51680708854858323072,83621143489848422977,135301852344706746049,218922995834555169026]
-- ghci> take 1 fibo
-- [0]

-- !Exercitiul 6

infiniteTruePrime :: [Bool]
infiniteTruePrime = map isPrime [2..]

isPrime :: Int -> Bool
isPrime n = isPrimeHelper n 2

isPrimeHelper :: Int -> Int -> Bool
isPrimeHelper n divisor | divisor * divisor > n = True
isPrimeHelper n divisor | n `mod` divisor == 0  = False
isPrimeHelper n divisor = isPrimeHelper n (divisor + 1)

-- ghci> take 20 infiniteTruePrime
-- [False,True,True,False,True,False,True,False,False,False,True,False,True,False,False,False,True,False,True,False]
-- ghci> take 10 infiniteTruePrime
-- [False,True,True,False,True,False,True,False,False,False]

-- !Exercitiul 7

infinitePrime :: [Int]
infinitePrime = filter prime [2..]

prime :: Int -> Bool
prime n | isPrime n = True
prime _ = False

-- ghci> take 10 infinitePrime
-- [2,3,5,7,11,13,17,19,23,29]
-- ghci> take 20 infinitePrime
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]