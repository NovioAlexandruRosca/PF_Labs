-- Nume:  Rosca Alexandru-David
-- Grupa: A4
-- Anul:  II
-- Laborator: XI

-- //////////////////////////////////////////////////////////////

-- import Prelude hiding (Left, Right)

-- data Arb = Nil | Node Int Arb Arb deriving (Show, Eq)

-- t1 :: Arb
-- t1 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 Nil Nil)

-- f :: Arb -> Int -> Arb
-- f (Node x (Node y (Node o a1 a2) a3) z) v = (Node x (Node y (Node v a1 a2) a3) z)

-- data Dir = L | R deriving (Show, Eq)

-- type Poz = [Dir]

-- at :: Arb -> Poz -> Arb
-- at a [] = a
-- at (Node _ a1 _) (L : p) = at a1 p
-- at (Node _ _ a2) (R : p) = at a2 p

-- change :: Arb -> Poz -> Int -> Arb
-- change Nil [] _ = error "Nu exista valoarea veche"
-- change Nil _ _ = error "Nu exista pozitia"
-- change (Node o a1 a2) [] v = Node v a1 a2
-- change (Node x a1 a2) (L : p) v = (Node x (change a1 p v) a2)
-- change (Node x a1 a2) (R : p) v = (Node x a1 (change a2 p v))

-- p = [L, L]

-- data Crumb = Left Int Arb | Right Int Arb deriving (Show, Eq)

-- type Trail = [Crumb]

-- type Zipper = (Arb, Trail)

-- goLeft :: Zipper -> Maybe Zipper
-- goLeft (Nil, _) = Nothing
-- goLeft (Node x a1 a2, t) = Just (a1, (Left x a2) : t)

-- goRight :: Zipper -> Maybe Zipper
-- goRight (Nil, _) = Nothing
-- goRight (Node x a1 a2, t) = Just (a2, (Right x a1) : t)

-- goUp :: Zipper -> Maybe Zipper
-- goUp (a, []) = Nothing
-- goUp (a, (Left x a2 : t)) = Just (Node x a a2, t)
-- goUp (a, (Right x a1 : t)) = Just (Node x a1 a, t)

-- change' :: Int -> Zipper -> Maybe Zipper
-- change' _ (Nil, _) = Nothing
-- change' v (Node x a1 a2, t) = Just (Node v a1 a2, t)

-- example1 :: Maybe Zipper
-- example1 = Just (t1, []) >>= goLeft >>= goUp

-- example2 :: Maybe Zipper
-- example2 = Just (t1, []) >>= goLeft >>= goRight >>= change' 10

-- example3 :: Maybe Zipper
-- example3 = Just (t1, []) >>= goRight >>= goUp >>= change' 20 >>= goRight

-- ghci> example1
-- Just (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil),[Left 1 (Node 3 Nil Nil)])
-- ghci> example1
-- Just (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil),[Left 1 (Node 3 Nil Nil)])
-- ghci> example2
-- Just (Node 42 Nil Nil,[Right 2 (Node 4 Nil Nil),Left 1 (Node 3 Nil Nil)])
-- ghci> example3
-- Nothing

-- //////////////////////////////////////////////////////////////

change :: [Int] -> Int -> Int -> [Int]
change [] _ _ = error "No such position"
change (hd : tl) 0 v = (v : tl)
change (hd : tl) i v = hd : (change tl (i - 1) v)

data Dir = Fwd deriving (Show, Eq)

type Pos = [Dir]

at :: [Int] -> Pos -> [Int]
at l [] = l
at (hd : tl) (Fwd : p) = at tl p

data Crumb = Forward Int deriving (Show, Eq)

type Trail = [Crumb]

type Zipper = ([Int], Trail)


[1,2,3]  []
[2,3]  [1]
[3]  [2,1]
[2,3] [1]

change' :: [Int] -> Pos -> Int -> Maybe [Int]
change' (hd : tl) [] v = Just (v : tl)
change' [] [] _ = Nothing
change' (hd : tl) (Fwd : p) v = change' tl p v >>= \newTail -> Just (hd : newTail)

goFwd :: Zipper -> Maybe Zipper
goFwd ([], _) = Nothing 
goFwd (hd : tl, t) = Just (tl, (Forward hd : t))

goBwd :: Zipper -> Maybe Zipper
goBwd (_, []) = Nothing
goBwd (l, Forward x : t) = Just (x : l, t)

change'' :: Zipper -> Int -> Maybe Zipper
change'' ([], _) v = Nothing
change'' (hd : tl, t) v = Just (v : tl, t)


main :: IO ()
main = do
    let zipper = ([1, 2, 3, 4, 5], []) :: Zipper
    let zipperFwd = zipper >>= goFwd >>= goFwd
    let zipperChange = zipperFwd >>= (`change''` 9)
    let zipperBwd = zipperChange >>= goBwd
       
    print zipperFwd        
    -- Just ([3, 4, 5], [Forward 2, Forward 1])
    print zipperChange
    -- Just ([9, 4, 5], [Forward 2, Forward 1])     
    print zipperBwd   
    -- ust ([2, 9, 4, 5], [Forward 1])

-- -- //////////////////////////////////////////////////////////////

-- import Prelude hiding (Left, Right)

-- data Tree = Node Int [Tree] deriving (Show, Eq)

-- data Dir = Down | Right deriving (Show, Eq)

-- type Zipper = ([Tree], [Crumb])

-- data Crumb = Crumb Int [Tree] [Tree] deriving (Show, Eq)

-- fromTree :: Tree -> Zipper
-- fromTree t = ([t], [])

-- goDown :: Zipper -> Maybe Zipper
-- goDown (Node x (t:ts) : zs, bs) = Just (t : ts, Crumb x [] zs : bs)
-- goDown _ = Nothing

-- goRight :: Zipper -> Maybe Zipper
-- goRight (t : ts, Crumb x ls rs : bs) = Just (ts, Crumb x (t : ls) rs : bs)
-- goRight _ = Nothing

-- change :: Int -> Zipper -> Maybe Zipper
-- change v (Node _ ts : zs, bs) = Just (Node v ts : zs, bs)
-- change _ _ = Nothing

-- goBack :: Zipper -> Maybe Zipper
-- goBack (t : ts, Crumb x ls rs : bs) = Just (Node x (reverse ls ++ [t] ++ rs) : ts, bs)
-- goBack _ = Nothing


-- atPos :: [Tree] -> [Dir] -> Int
-- atPos [Node x _] [] = x
-- atPos (Node _ (t:ts) : zs) (Down:ds) = atPos (t:ts) ds
-- atPos (t:ts) (Right:ds) = atPos ts ds
-- atPos _ _ = -1


-- t :: Tree
-- t = Node 10 [Node 3 [],
--     Node 4 [Node 1 [], Node 2 [], Node 22 [], Node 33[] ],
--     Node 5 [], Node 6 []]

-- pos1 = [Down, Right, Down, Right, Right]

-- ghci> print $ atPos [t] pos1
-- Nothing
    



