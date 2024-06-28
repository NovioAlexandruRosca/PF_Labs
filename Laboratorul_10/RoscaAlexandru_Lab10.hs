import System.Random
import Data.Array.IO
import Control.Monad.State

-- Exercitiul 1

type EuclidState = (Int, Int, Int, Int, Int, Int)

euclidExtended :: State EuclidState Int
euclidExtended = do
    (a, b, x1, y1, x2, y2) <- get
    if b == 0 then do
      return a
    else do
      let q = a `div` b
      let r = a `mod` b
      put (b, r, x2, y2, x1 - q * x2, y1 - q * y2)
      euclidExtended


runEuclidExtended :: Int -> Int -> ((Int, Int, Int), EuclidState)
runEuclidExtended a b = runState euclidExtended (a, b, 1, 0, 0, 1)


-- main :: IO ()
-- main = do
--     let ((gcd, x, y), _) = runEuclidExtended 20 42
--     putStrLn ("Extended Euclid: " ++ show gcd)


-- Extended Euclid: 2


-- Exercitiul 2

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Enum)
type Rank = Int
type Card = (Rank, Suit)
type Deck = [Card]

randomRState :: (Int, Int) -> State StdGen Int
randomRState range = state $ randomR range

createCardDeck :: Deck
createCardDeck = [(rank, suit) | suit <- [Clubs, Diamonds, Hearts, Spades], rank <- [1..13]]

shuffleCardDeck :: [a] -> IO [a]
shuffleCardDeck deck = do
    array <- newIOArray deckSize deck
    forM [1..deckSize] $ \i -> do
        j <- randomRIO (i, deckSize)
        valueAtIndexI <- readArray array i
        valueAtIndexJ <- readArray array j
        writeArray array j valueAtIndexI
        return valueAtIndexJ
  where
    deckSize = length deck
    newIOArray :: Int -> [a] -> IO (IOArray Int a)
    newIOArray len elements = newListArray (1, len) elements

-- main :: IO ()
-- main = do
--     let deck = createCardDeck
--     shuffledCardDeck <- shuffleCardDeck deck
--     print shuffledCardDeck


-- [(5,Diamonds),(2,Clubs),(11,Hearts),(8,Spades),(6,Diamonds),(1,Diamonds),(3,Clubs),(13,Spades),(7,Hearts),(12,Clubs),(10,Spades),(9,Hearts),(4,Diamonds),(3,Hearts),(13,Hearts),(5,Spades),(7,Clubs),(2,Diamonds),(8,Clubs),(11,Clubs),(4,Spades),(12,Hearts),(6,Spades),(9,Clubs),(1,Hearts),(10,Diamonds),(7,Spades),(5,Clubs),(3,Diamonds),(13,Clubs),(11,Diamonds),(6,Clubs),(12,Spades),(4,Hearts),(2,Spades),(9,Spades),(1,Clubs),(10,Hearts),(13,Diamonds),(11,Spades),(8,Diamonds),(3,Spades),(12,Diamonds),(7,Diamonds),(5,Hearts),(2,Hearts),(10,Clubs),(1,Spades),(4,Clubs),(9,Diamonds),(8,Hearts),(6,Hearts)]

-- Exercitiul 3

data Op = Plus | Mult deriving (Show, Eq)
data Elem = Number Int | Operator Op deriving (Show, Eq)
type RPNExp = [Elem]
type Stack = [Int]


evalElem :: Elem -> State Stack ()
evalElem (Number n) = modify (n :)
evalElem (Operator op) = do
  stack <- get
  case stack of
    (x:y:ys) -> do
      let result = case op of
                     Plus -> x + y
                     Mult -> x * y
      put (result:ys)
    _ -> error "Not enough operands"


evalRPN :: RPNExp -> State Stack Int
evalRPN expr = do
  mapM_ evalElem expr
  final <- get
  case final of
    [result] -> return result
    _        -> error "Invalid expression"


expression :: RPNExp
expression = [Number 3, Number 4, Operator Mult, Number 5, Operator Plus]

-- main :: IO ()
-- main = do
--   let res = evalState (evalRPN expression) []
--   print res


-- 17