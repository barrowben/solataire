import System.Random
import Data.List

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Enum, Show, Bounded)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Enum, Show, Bounded)
data Card = Card Pip Suit
instance Show Card where
    show (Card p s) = "(" ++ show p ++ "," ++ show s ++ ")"

type Deck = [Card]

type Foundation = [Deck]
type Reserve = Deck
type Column = Deck

data EOBoard = EOBoard
         Foundation  -- foundations
         [Column]  -- columns
         Reserve   -- reserve

instance Show EOBoard where
    show (EOBoard f cs r) =
        "EOBoard\n" ++
        "Foundations  " ++ show f ++
        "\nColumns\n" ++
        show (head cs) ++ "\n" ++
        show (cs!!1) ++ "\n" ++
        show (cs!!2) ++ "\n" ++
        show (cs!!3) ++ "\n" ++
        show (cs!!4) ++ "\n" ++
        show (cs!!5) ++ "\n" ++
        show (cs!!6) ++ "\n" ++
        show (cs!!7) ++ "\n" ++                
        "\nReserve  " ++ show r

pack :: Deck
pack = [Card pip suit | pip <- [Ace .. King], suit <- [Clubs .. Spades]]

fourCards :: Deck
fourCards = [Card Three Hearts, Card Two Clubs, Card Seven Diamonds, Card King Clubs]

sixCards :: Deck
sixCards = [Card Ace Clubs, Card Two Hearts, Card Three Diamonds, Card Ace Hearts, Card Six Hearts, Card Seven Diamonds]

shuffledPack :: Deck
shuffledPack = shuffle 1234 pack

testFound :: Foundation
testFound = []

testCol :: Column
testCol = sixCards

testReserve :: Reserve
testReserve = fourCards

testEO :: EOBoard
testEO = EOBoard testFound [testCol, testCol, testCol, testCol, testCol, testCol, testCol, testCol] testReserve

-- test cards
tCard = Card King Clubs
tCard' = Card Ace Hearts

sCard :: Card -> Card
sCard (Card p s)
    | p == King = Card Ace s
    | otherwise = Card (succ p) s

pCard :: Card -> Card
pCard (Card p s)
    | p == Ace = Card King s
    | otherwise = Card (pred p) s

isAce :: Card -> Bool
isAce (Card p _) | p == Ace = True 
                 | otherwise = False

isKing :: Card -> Bool
isKing (Card p _) | p == King = True 
                  | otherwise = False

-- Shuffle deck of cards
cmp (x1,y1) (x2,y2) = compare y1 y2
shuffle :: Int -> [a] -> [a]
shuffle n xs = [ x | (x, n) <- sortBy cmp
                          (zip xs (randoms (mkStdGen n) :: [Int]))]

