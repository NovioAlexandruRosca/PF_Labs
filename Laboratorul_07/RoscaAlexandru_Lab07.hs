-- Nume:  Rosca Alexandru-David
-- Grupa: A4
-- Anul:  II
-- Laborator: VII

-- //////////////////////////////////////////////////////////////

-- !Exercitiul 0.1

-- main :: IO ()
-- main = putStrLn "Hello, World!"

-- main :: IO ()
-- main = putStrLn "Hello, World!" >> putStrLn "All good."

-- !Exercitiul 0.2

-- main :: IO ()
-- main = putStrLn "Hello, World!" >> putStrLn "All good." >> putStrLn "Triple"

-- main :: IO ()
-- main = putStrLn "What is your name?" >>
--        getLine >>
--        putStrLn "Hello, ...!"

-- main :: IO ()
-- main = putStrLn "What is your name?" >>
--         getLine >>=
--         (\name -> putStrLn ("Hello, " ++ name ++ "!"))


-- !Exercitiul 0.3

-- main :: IO ()
-- main = putStrLn "What is your name?" >>
--        getLine >>= 
--        (\name -> putStrLn "What is your surname?" >>
--                  getLine >>= 
--                  (\surname -> putStrLn ("Hello, " ++ name ++ " " ++ surname ++ "!"))

-- !Exercitiul 0.4

-- main :: IO ()
-- main = do
--     putStrLn "What is your name?"
--     name <- getLine 
--     putStrLn "What is your surname?"
--     surname <- getLine
--     putStrLn ("Hello, " ++ name ++ " " ++ surname ++ "!")

-- !Exercitiul 0.5

-- main :: IO ()
-- main = putStrLn "What is your name?" >>
--         getLine >>=
--         (\name -> putStrLn ("Hello, " ++ name ++ "!") >> main)

-- !Exercitiul 0.6

-- main :: IO ()
-- main = putStrLn "What is your name?" >>
--        getLine >>= 
--        (\name -> putStrLn "What is your surname?" >>
--                  getLine >>= 
--                  (\surname -> main))

-- !Exercitiul 0.7

-- main :: IO ()
-- main = do
--         putStrLn "What is your name?"
--         name <- getLine
--         putStrLn "What is your surname?"
--         surname <- getLine
        --    putStrLn ("Hello" ....)
--         if(name == "" || surname == "")
--             then 
--                 return()
--         else
--             main

-- !Exercitiul 0.8

-- import Data.Char (ord, chr)

-- upperChar :: Char -> Char
-- upperChar c = chr (ord c + (-32))

-- parseWord :: [Char] -> [Char] 
-- parseWord xs = map upperChar xs

-- main :: IO ()
-- main = do
--         putStrLn "What is your name?"
--         name <- getLine
--         let uppercasedName = parseWord name
--         putStrLn "What is your surname?"
--         surname <- getLine
--         let uppercasedSurname = parseWord surname
--         putStrLn ("Hello " ++ uppercasedName ++ " " ++ uppercasedSurname ++ "!")

-- What is your name?
-- ana
-- What is your surname?
-- mere
-- Hello ANA MERE!

-- !Exercitiul 0.9

-- openFile :: FilePath -> IOMode -> IO Handle
-- hGetContents :: Handle -> IO String 
-- hGetLine :: Handle -> IO String
-- hClose :: Handle -> IO () 
-- getArgs :: IO [String] 
-- getProgName :: IO String
-- hPutStr :: Handle -> String -> IO ()

-- !Exercitiul 0.10

-- import System.IO

-- main :: IO ()
-- main = do
--     handle <- openFile "exemplu.txt" ReadMode 
--     contents <- hGetContents handle           
--     putStrLn contents                         
--     hClose handle                             

-- !Exercitiul 0.11

-- import System.IO
-- import System.Environment

-- main :: IO ()
-- main = do
--     args <- getArgs                             
--     case args of
--         [name] -> do                       
--             handle <- openFile name ReadMode 
--             contents <- hGetContents handle      
--             putStrLn contents                     
--             hClose handle                     

-- PS D:\Fac\pf\Laboratorul_07> .\exec.exe exemplu.txt
-- lmao

-- !Exercitiul 0.12

-- import System.IO
-- import System.Environment

-- main :: IO ()
-- main = do
--     args <- getArgs                             
--     case args of
--         [name] -> do                       
--             handle <- openFile name ReadMode 
--             contents <- hGetContents handle      
--             putStrLn contents                     
--             hClose handle  
--         _ -> do 
--             progName <- getProgName
--             putStrLn(progName ++ " NUME FISIER") 

-- PS D:\Fac\pf\Laboratorul_07> .\exec.exe exemplu.txt a
-- exec.exe NUME FISIER

-- !Exercitiul 0.13

-- import Data.Char (ord, chr)
-- import System.IO
-- import System.Environment

-- upperChar :: Char -> Char
-- upperChar c = chr (ord c + (-32))

-- parseWord :: [Char] -> [Char] 
-- parseWord xs = map upperChar xs

-- main :: IO ()
-- main = do
--     args <- getArgs                             
--     case args of
--         [name] -> do                       
--             handle <- openFile name ReadMode 
--             contents <- hGetContents handle      
--             let uppercasedContent = parseWord contents
--             putStrLn uppercasedContent                     
--             hClose handle  
--         _ -> do 
--             progName <- getProgName
--             putStrLn(progName ++ " NUME FISIER") 

-- PS D:\Fac\pf\Laboratorul_07> .\exec.exe exemplu.txt  
-- LMAO

-- !Exercitiul 0.14

-- binarySearchGuess :: Int -> Int -> Int -> IO ()
-- binarySearchGuess target low high | low == high = putStrLn ("Numarul este " ++ show low)
-- binarySearchGuess target low high = do
--     let mid = (low + high) `div` 2
--     putStrLn ("Numarul de ghicit este >= " ++ show mid ++ "?")
--     answer <- getLine
--     case answer of
--         "Da" -> binarySearchGuess target (mid + 1) high
--         "Nu" ->  binarySearchGuess target low mid
--         _ -> do
--             putStrLn "Nu am inteles."
--             binarySearchGuess target low high


-- main :: IO ()
-- main = binarySearchGuess 0 1 100

-- PS D:\Fac\pf\Laboratorul_07> .\exec.exe exemplu.txt
-- Numarul de ghicit este >= 50?
-- Da
-- Numarul de ghicit este >= 75?
-- Nu
-- Numarul de ghicit este >= 63?
-- Nu
-- Numarul de ghicit este >= 57?
-- Nu
-- Numarul de ghicit este >= 54?
-- Nu
-- Numarul de ghicit este >= 52?
-- Nu
-- Numarul de ghicit este >= 51?
-- Nu
-- Numarul este 51


