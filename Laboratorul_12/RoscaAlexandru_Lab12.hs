-- Rosca Alexandru David
-- 2A4

type Id = String

data Term = Var Id
          | App Term Term
          | Lambda Id Term 
          deriving (Show, Eq)


-- Exercitiul 0.1
term :: Term
term = Lambda "x" (Lambda "y" (Var "x"))

-- Exericitul 0.2

subst :: Id -> Term -> Term -> Term
subst id term (Var id')  | id == id' = term
                         | True      = Var id'

subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)

subst id term (Lambda id' term')  | id == id' = Lambda id' term'
                                  | True      = Lambda id' (subst id term term')



test1 :: Bool
test1 = subst "x" (Var "y") (Var "x") == Var "y"

test2 :: Bool
test2 = subst "y" (Var "z") (Var "x") == Var "x"

test3 :: Bool
test3 = subst "y" (Var "z") (App (Var "x") (Var "y")) == App (Var "x") (Var "z")

test4 :: Bool
test4 = subst "y" (Var "z") (App (Var "y") (Var "x")) == App (Var "z") (Var "x")

test5 :: Bool
test5 = subst "x" (Lambda "z" (Var "z")) (Lambda "x" (App (Var "y") (Var "x"))) == Lambda "x" (App (Var "y") (Var "x"))

test6 :: Bool
test6 = subst "x" (Lambda "z" (Var "z")) (Lambda "y" (App (Var "y") (Var "x"))) == Lambda "y" (App (Var "y") (Lambda "z" (Var "z")))

-- ghci> test1
-- True
-- ghci> test2
-- True
-- ghci> test3
-- True
-- ghci> test4
-- True
-- ghci> test5
-- True
-- ghci> test6
-- True

-- Exercitiul 0.3

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl)
    | id == hd  = remove id tl
    | otherwise = hd : remove id tl

-- Exercitul 0.4

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = filter (/= id) (free term)

-- Exercitul 0.5

vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) = id : vars(term)

-- Exercitiul 0.6

fresh' :: [Id] -> Int -> Id
fresh' ids index = if ("n" ++ show index) `elem` ids 
                   then fresh' ids (index + 1) 
                   else "n" ++ show index

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

-- Exericitul 0.7

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _ | id == id' = term
                            | otherwise = Var id'

casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid)

casubst id term (Lambda id' term') avoid | id == id'              = Lambda id' term'
                                         | id' `elem` (free term) = 
            let id'' = fresh (avoid ++ vars term ++ vars term') in Lambda id'' (casubst id term (casubst id' (Var id'') term' (id'':avoid)) (id'':avoid))
                                         | True                   = Lambda id' (casubst id term term' (id':avoid))

                                         
testCasubst1 = casubst "x" (Var "y") (Var "x") [] == Var "y"
testCasubst2 = casubst "y" (Var "z") (Var "x") [] == Var "x"
testCasubst3 = casubst "y" (Var "z") (App (Var "x") (Var "y")) [] == App (Var "x") (Var "z")
testCasubst4 = casubst "y" (Var "z") (App (Var "y") (Var "x")) [] == App (Var "z") (Var "x")
testCasubst5 = casubst "x" (Lambda "z" (Var "z")) (Lambda "x" (App (Var "y") (Var "x"))) [] == Lambda "x" (App (Var "y") (Var "x"))
testCasubst6 = casubst "x" (Lambda "z" (Var "z")) (Lambda "y" (App (Var "y") (Var "x"))) [] == Lambda "y" (App (Var "y") (Lambda "z" (Var "z")))
testCasubst7 = casubst "y" (Var "x") (Lambda "x" (Var "y")) [] == Lambda "n0" (Var "x")

-- ghci> testCasubst1
-- True
-- ghci> testCasubst2
-- True
-- ghci> testCasubst3
-- True
-- ghci> testCasubst4
-- True
-- ghci> testCasubst5
-- True
-- ghci> testCasubst6
-- True
-- ghci> testCasubst7
-- True

-- Exercitiul 0.8

reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var _) _ = Nothing
reduce1' (App (Lambda id term) term') avoid = Just (casubst id term' term avoid)
reduce1' (App term1 term2) avoid = case reduce1' term1 avoid of
                                        Nothing -> case reduce1' term2 avoid of
                                            Nothing -> Nothing
                                            Just term2' -> Just (App term1 term2')
                                        Just term1' -> Just (App term1' term2)
reduce1' (Lambda id term) avoid = case reduce1' term avoid of
                                    Nothing -> Nothing
                                    Just term' -> Just (Lambda id term')

reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)


main :: IO ()
main = do
    let x = Var "x"
    let y = Var "y"
    let z = Var "z"
    let term1 = Lambda "x" x
    let term2 = App term1 term1
    let term3 = Lambda "y" (Lambda "x" term2)
    let term4 = App term3 term1
    let ex1 = reduce1 term1 -- Nothing
    let ex2 = reduce1 term2 -- Just (\x.x)
    let ex3 = reduce1 term3 -- Just \y.\x.(\x.x)
    let ex4 = reduce1 term4 -- Care este rezultatul?
    putStrLn $ "reduce1 term1: " ++ show ex1
    putStrLn $ "reduce1 term2: " ++ show ex2
    putStrLn $ "reduce1 term3: " ++ show ex3
    putStrLn $ "reduce1 term4: " ++ show ex4

-- reduce1 term1: Nothing
-- reduce1 term2: Just (Lambda "x" (Var "x"))
-- reduce1 term3: Just (Lambda "y" (Lambda "x" (Lambda "x" (Var "x"))))
-- reduce1 term4: Just (Lambda "x" (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))))

-- Exercitiul 0.9

reduce :: Term -> Term
reduce term = case reduce1' term (vars term) of
    Nothing -> term
    Just term' -> reduce term'

-- Exericitul 0.10

reduceFor :: Int -> Term -> Term
reduceFor 0 term = term
reduceFor n term = case reduce1 term of
    Nothing -> term
    Just term' -> reduceFor (n - 1) term'

-- Exericitul 0.11


tTRUE = Lambda "x" (Lambda "y" (Var "x"))
tAND = Lambda "x" (Lambda "y" (App (App (Var "x") (Var "y")) (Var "x")))
tFALSE = Lambda "x" (Lambda "y" (Var "y"))
tOR = Lambda "x" (Lambda "y" (App (App (Var "x") (Var "x")) (Var "y")))
tNOT = Lambda "x" (App (App (Var "x") tFALSE) tTRUE)
tITE = Lambda "cond" (Lambda "true" (Lambda "false" (App (App (App (Var "cond") (Var "true")) (Var "false"))))) 