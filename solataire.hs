import System.Random
import Data.List

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Enum, Show, Bounded)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Enum, Show, Bounded)
data Card = Card Pip Suit
instance Show Card where
    show (Card p s) = "(" ++ show p ++ " of " ++ show s ++ ")"

type Deck = [Card]
type Foundation = [Card]
type Reserve = [Card]
type Column = [Card]

data Board = EOBoard
         [Foundation]
         [Column]
         Reserve

instance Show Board where
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
cmp (x1, y1) (x2, y2) = compare y1 y2
shuffle :: Int -> [a] -> [a]
shuffle n xs = [ x | (x, n) <- sortBy cmp
                          (zip xs (randoms (mkStdGen n) :: [Int]))]

-- Takes seed and generates random starting setup
eODeal :: Int -> Board
eODeal seed = EOBoard fnds cols res
    where
        shuffled = shuffle seed pack
        fnds  = [[],[],[],[]]
        cols  = [take 6 shuffled,
                take 6 (drop 6 shuffled),
                take 6 (drop 12 shuffled),
                take 6 (drop 18 shuffled),
                take 6 (drop 24 shuffled),
                take 6 (drop 30 shuffled),
                take 6 (drop 36 shuffled),
                take 6 (drop 42 shuffled)]
        res   = [shuffled!!48,
                shuffled!!49,
                shuffled!!50,
                shuffled!!51]