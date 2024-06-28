-- Nume:  Rosca Alexandru-David
-- Grupa: A4
-- Anul:  II
-- Laborator: III
-- Exercises Done: All

-- //////////////////////////////////////////////////////////////

-- !Exercitiul 1.2 / 1.3

data Colors = Red
            | Green
            | Blue
            deriving (Show, Eq)

data MobileDevice = Smartphone Colors
                  | Laptop Colors
                  | Tablet Colors
                   deriving (Show, Eq)

-- !Exercitiul 1.4

deviceColor :: MobileDevice -> Colors
deviceColor (Smartphone color) = color
deviceColor (Laptop color) = color
deviceColor (Tablet color) = color

-- ghci> deviceColor (Smartphone Red)
-- Red
-- ghci> deviceColor (Smartphone Green)
-- Green
-- ghci> deviceColor (Tablet Blue)
-- Blue

-- //////////////////////////////////////////////////////////////

-- !Exercitiul 2.1

data Arb = Frunza
         | Nod Integer Arb Arb 
         deriving (Show, Eq)

-- !Exercitiul 2.2

isBst :: Arb -> Bool
isBst Frunza = True
isBst (Nod valueParent Frunza Frunza) = True
isBst (Nod valueParent Frunza (Nod valueRight subArbS subArbD)) | valueParent > valueRight = False
isBst (Nod valueParent Frunza (Nod valueRight subArbS subArbD)) = isBst(Nod valueRight subArbS subArbD)
isBst (Nod valueParent (Nod valueLeft subArbS subArbD) Frunza) | valueParent < valueLeft = False
isBst (Nod valueParent (Nod valueLeft subArbS subArbD) Frunza) = isBst(Nod valueLeft subArbS subArbD)
isBst (Nod valueParent (Nod valueLeft subArbS1 subArbD1) (Nod valueRight subArbS2 subArbD2)) | (valueParent < valueLeft) || (valueParent > valueRight) = False
isBst (Nod valueParent (Nod valueLeft subArbS1 subArbD1) (Nod valueRight subArbS2 subArbD2)) = isBst(Nod valueLeft subArbS1 subArbD1) && isBst(Nod valueRight subArbS2 subArbD2)

--  !    100          
--  !   /   \          
--  !  50   150  

-- ghci> isBst(Nod 100 (Nod 50 Frunza Frunza)(Nod 150 Frunza Frunza))
-- True

--  !    100
--  !   /   \ 
--  !  50   75

-- ghci> isBst(Nod 100 (Nod 50 Frunza Frunza)(Nod 75 Frunza Frunza))
-- False

-- Mai complicat:

-- !      5          
-- !    /   \          
-- !   3     7
-- !  / \   /
-- ! 2   4 6   

-- ghci> isBst(Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza))
-- True

-- !      5          
-- !    /   \          
-- !   4     7
-- !  / \   /
-- ! 2   3 6 

-- !Problema e la subarborele 4-2-3

-- ghci> isBst(Nod 5 (Nod 4 (Nod 2 Frunza Frunza) (Nod 3 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza))
-- False

-- !Exercitiul 2.3

data TypeWithErr a = Result a
                   | Error String
                   deriving (Show, Eq)

search :: Arb -> Integer -> TypeWithErr Bool
search arb _ | not (isBst arb) = Error "Arborele nu este arbore binar de cautare!"
search arb value = searchHelper arb value

searchHelper :: Arb -> Integer -> TypeWithErr Bool
searchHelper Frunza value = Result False
searchHelper (Nod nodeValue arb1 arb2) value | nodeValue == value = Result True
searchHelper (Nod nodeValue arb1 arb2) value = case (searchHelper arb1 value, searchHelper arb2 value) of
                                                (Result True, _) -> Result True
                                                (_, Result True) -> Result True
                                                (Error err1, _) -> Error err1
                                                (_, Error err2) -> Error err2
                                                _ -> Result False

-- !      5          
-- !    /   \          
-- !   3     7
-- !  / \   /
-- ! 2   4 6 

-- ghci> search (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza)) 5
-- Result True
-- ghci> search (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza)) 100
-- Result False

-- !      5          
-- !    /   \          
-- !   4     7
-- !  / \   /
-- ! 2   3 6 


-- ghci> search (Nod 5 (Nod 4 (Nod 2 Frunza Frunza) (Nod 3 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza)) 100
-- Error "Arborele nu este arbore binar de cautare!"

-- !Exercitiul 2.4

insert :: Arb -> Integer -> TypeWithErr Arb
insert arb _ | not (isBst arb) = Error "Arborele nu este arbore binar de cautare!"
insert arb value = insertHelper arb value

insertHelper :: Arb -> Integer -> TypeWithErr Arb
insertHelper Frunza value = Result (Nod value Frunza Frunza)
insertHelper (Nod nodeValue arb1 arb2) value | value == nodeValue = Error "Valoarea exista deja in arborele binar de cautare!"
insertHelper (Nod nodeValue arb1 arb2) value | value < nodeValue = case insertHelper arb1 value of
                                                                    (Error error) -> Error error
                                                                    (Result arb) -> Result (Nod nodeValue arb arb2)
insertHelper (Nod nodeValue arb1 arb2) value = case insertHelper arb2 value of
                                                (Error error) -> Error error
                                                (Result arb) -> Result (Nod nodeValue arb1 arb)


--  !    Nimic -> 5

-- ghci> insert Frunza 5
-- Result (Nod 5 Frunza Frunza)

-- !     Adaugam o valoare care exista deja

-- ghci> insert (Nod 5 Frunza Frunza) 5
-- Error "Valoarea exista deja in arborele binar de cautare!"

-- !           5
-- !   5  ->  /
-- !         1

-- ghci> insert (Nod 5 Frunza Frunza) 1
-- Result (Nod 5 (Nod 1 Frunza Frunza) Frunza)

-- !              5
-- !      5      /
-- !     /  ->  2
-- !    2      /
-- !          1

-- ghci> insert (Nod 5 (Nod 2 Frunza Frunza) Frunza) 1
-- Result (Nod 5 (Nod 2 (Nod 1 Frunza Frunza) Frunza) Frunza)

-- !                 5
-- !         5      /
-- !        /      3
-- !       3  ->  /
-- !      /      1
-- !     1       \
-- !              2      

-- ghci> insert (Nod 5 (Nod 3 (Nod 1 Frunza Frunza) Frunza) Frunza) 2
-- Result (Nod 5 (Nod 3 (Nod 1 Frunza (Nod 2 Frunza Frunza)) Frunza) Frunza)

-- ! Exercitiul 2.5

data Option = Min 
            | Max 
            deriving (Show)

extreme :: Arb -> Option -> TypeWithErr Integer
extreme arb _ | not (isBst arb) = Error "Arborele nu este arbore binar de cautare!"
extreme arb option = extremeHelper arb option

extremeHelper :: Arb -> Option -> TypeWithErr Integer
extremeHelper Frunza _ = Error "Nu avem elemente" 
extremeHelper (Nod nodeValue arb1 arb2) Min = case extremeHelper arb1 Min of
                                             (Error error) -> Result nodeValue
                                             (Result value) -> Result (min value nodeValue)
extremeHelper (Nod nodeValue arb1 arb2) Max = case extremeHelper arb2 Max of
                                             (Error error) -> Result nodeValue
                                             (Result value) -> Result (max value nodeValue)

-- ghci> extreme Frunza Min
-- Error "Nu avem elemente"
-- ghci> extreme (Nod 5 Frunza Frunza) Min
-- Result 5

-- !      5          
-- !    /   \          
-- !   3     7
-- !  / \   /
-- ! 2   4 6 

-- ghci> extreme (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza)) Min
-- Result 2
-- ghci> extreme (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza)) Max
-- Result 7

-- ! Exercitiul 2.6

removeMax :: Arb -> Arb
removeMax Frunza = Frunza
removeMax (Nod nodeValue Frunza Frunza) = Frunza
removeMax (Nod nodeValue arbS Frunza) = arbS
removeMax (Nod nodeValue arbS arbD) = Nod nodeValue arbS (removeMax arbD)

-- !      5              5
-- !    /   \          /   \
-- !   3     7   ->   3     7 
-- !  / \   / \      / \   / 
-- ! 2   4 6   8    2   4 6   

-- ghci> removeMax (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 8 Frunza Frunza)))
-- Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza)

-- !      5           5 
-- !    /   \        / \  
-- !   3     7  ->  3   6
-- !  / \   /      / \
-- ! 2   4 6      2   4

-- ghci> removeMax (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza))
-- Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 6 Frunza Frunza)

-- !      5           5
-- !    /   \        /  
-- !   3     6  ->  3
-- !  / \          / \
-- ! 2   4        2   4

-- ghci> removeMax (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 6 Frunza Frunza))
-- Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) Frunza

-- !       5          
-- !     /           3       
-- !    3     ->   /  \
-- !  /  \        2    4
-- ! 2    4  

-- ghci> removeMax (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) Frunza)
-- Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)

-- !    5  ->  Nimic 

-- ghci> removeMax (Nod 1 Frunza Frunza)
-- Frunza

-- ! Exercitul 2.7

remove :: Arb -> Integer -> Arb
remove Frunza value = Frunza
remove (Nod nodeValue Frunza Frunza) value | value == nodeValue = Frunza
remove (Nod nodeValue arbS Frunza) value   | value == nodeValue = arbS
remove (Nod nodeValue Frunza arbD) value   | value == nodeValue = arbD
remove (Nod nodeValue arbS arbD) value     | value == nodeValue = removeHelper arbS arbD
remove (Nod nodeValue arbS arbD) value     | value > nodeValue = Nod nodeValue arbS (remove arbD value)
remove (Nod nodeValue arbS arbD) value     = Nod nodeValue (remove arbS value) arbD

removeHelper :: Arb -> Arb -> Arb
removeHelper arbS1 (Nod nodeValue Frunza arbD) = Nod nodeValue arbS1 arbD
removeHelper arbS1 (Nod nodeValue arbS arbD) = Nod nodeValue (removeHelper arbS1 arbS) arbD

-- !      5              5
-- !    /   \          /   \
-- !   3     7   ->   3     7 
-- !  / \   / \      / \   / 
-- ! 2   4 6   8    2   4 6   

-- ghci> remove (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 8 Frunza Frunza))) 8
-- Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) Frunza)

-- !      5              5
-- !    /   \          /   \
-- !   3     7   ->   3     8 
-- !  / \   / \      / \   / 
-- ! 2   4 6   8    2   4 6   

-- ghci> remove (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 8 Frunza Frunza))) 7
-- Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 8 (Nod 6 Frunza Frunza) Frunza)

-- !      5              5
-- !    /   \          /   \
-- !   3     7   ->   3     7 
-- !  / \   / \      / \     \
-- ! 2   4 6   8    2   4     8

-- ghci> remove (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 8 Frunza Frunza))) 6
-- Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 Frunza (Nod 8 Frunza Frunza))

-- !                        7
-- !      5               /   \
-- !    /   \            6     8
-- !   3     7   ->     /
-- !  / \   / \        3     
-- ! 2   4 6   8     /  \
-- !                2    4     

-- ghci> remove (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 8 Frunza Frunza))) 5
-- Nod 7 (Nod 6 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) Frunza) (Nod 8 Frunza Frunza)

-- ! Exercitiul 2.8

preOrder :: Arb -> [Integer]
preOrder Frunza = []
preOrder (Nod nodeValue arbS arbD) = nodeValue : preOrder arbS ++ preOrder arbD

inOrder :: Arb -> [Integer]
inOrder Frunza = []
inOrder (Nod nodeValue arbS arbD) = inOrder arbS ++ nodeValue : inOrder arbD

postOrder :: Arb -> [Integer]
postOrder Frunza = []
postOrder (Nod nodeValue arbS arbD) = postOrder arbS ++ postOrder arbD ++ [nodeValue]

-- !      5               
-- !    /   \         
-- !   3     7   
-- !  / \   / \             
-- ! 2   4 6   8                      

-- ghci> preOrder (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 8 Frunza Frunza)))
-- [5,3,2,4,7,6,8]

-- ghci> inOrder (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 8 Frunza Frunza)))
-- [2,3,4,5,6,7,8]

-- ghci> postOrder (Nod 5 (Nod 3 (Nod 2 Frunza Frunza) (Nod 4 Frunza Frunza)) (Nod 7 (Nod 6 Frunza Frunza) (Nod 8 Frunza Frunza)))
-- [2,4,3,6,8,7,5]

-- //////////////////////////////////////////////////////////////

-- ! Exercitiul 3.1

data Exp = Const Bool 
         | Var String
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
         deriving (Show, Eq)

simplify :: Exp -> Exp
simplify (Const value) = Const value
simplify (Var value) = Var value
simplify (And (Const True) expr) = simplify expr
simplify (And expr (Const True)) = simplify expr
simplify (And (Const False) expr) = Const False
simplify (And expr (Const False)) = Const False
simplify (And expr1 (Not expr2)) | expr1 == expr2 = Const False
simplify (And expr1 expr2) | expr1 == expr2 = expr1
simplify (And expr1 (Or expr2 expr3))| expr1 == expr2 = expr1
simplify (And expr1 (Or expr2 expr3))| expr1 == expr3 = expr1
simplify (Not (And expr1 expr2)) = Or (Not expr1) (Not expr2)
simplify (And expr1 expr2) = simplify (And (simplify expr1) (simplify expr2))
simplify (Or (Const False) expr) = simplify expr
simplify (Or (Const True) expr) = Const True
simplify (Or expr1 expr2) | expr1 == expr2 = expr1
simplify (Or (Not expr1) expr2) | expr1 == expr2 = Const True
simplify (Or expr1 (Not expr2)) | expr1 == expr2 = Const True
simplify (Or expr1 (And expr2 expr3)) | expr1 == expr2 = expr1
simplify (Or expr1 (And expr2 expr3)) | expr1 == expr3 = expr1
simplify (Not (Or expr1 expr2)) = And (Not expr1) (Not expr2)   
simplify (Or expr1 expr2) = simplify(Or (simplify expr1) (simplify expr2))

-- ghci> simplify (And (Var "x") (Or (Const True) (Not (Var "y"))))
-- And (Var "x") (Const True)
-- ghci> simplify (Or (And (Const True) (Const True)) (And (Var "y") (Const True)))
-- Const True
-- ghci> simplify (Or (And (Const True) (Var "x")) (And (Var "x") (Const True)))
-- Var "x"
-- ghci> simplify (Or (And (Const False) (Var "x")) (And (Var "y") (Const True)))
-- Var "y"
-- ghci> simplify (Not (Or (And (Var "x") (Var "y")) (Or (Not (Var "x")) (Not (Var "y")))))
-- And (Not (And (Var "x") (Var "y"))) (Not (Or (Not (Var "x")) (Not (Var "y"))))

-- ! Exercitiul 3.2

toCNF :: Exp -> Exp
toCNF (Const value) = Const value
toCNF (Var value) = Var value
toCNF (And expr1 expr2) = And (toCNF expr1) (toCNF expr2)
toCNF (Or expr1 expr2) = distribute (toCNF expr1) (toCNF expr2)
toCNF (Not (Const value)) = Not (Const value)
toCNF (Not (Var value)) = Not (Var value)
toCNF (Not (Not expr)) = toCNF expr
toCNF (Not (And expr1 expr2)) = Or (toCNF (Not expr1)) (toCNF (Not expr2))
toCNF (Not (Or expr1 expr2)) = And (toCNF (Not expr1)) (toCNF (Not expr2))

distribute :: Exp -> Exp -> Exp
distribute (And p q) r = And (distribute p r) (distribute q r)
distribute p (And q r) = And (distribute p q) (distribute p r)
distribute p q = Or p q

-- ghci> toCNF ( Or (And (Var "x") (Var "y")) (Or (Not (Var "x")) (Not (Var "y"))))
-- And (Or (Var "x") (Or (Not (Var "x")) (Not (Var "y")))) (Or (Var "y") (Or (Not (Var "x")) (Not (Var "y"))))

-- //////////////////////////////////////////////////////////////

data Expr = Cons Float 
          | Let String 
          | Suma Exp Exp 
          | Prod Exp Exp 
          deriving (Show, Eq)

data Stmt = Skip
          | Seq Stmt Stmt
          | Assign String Expr
          | While Exp Stmt
          deriving(Show)
