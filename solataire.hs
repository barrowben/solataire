import System.Random
import Data.List

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Enum, Show, Bounded)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Enum, Show, Bounded)
data Card = Card Pip Suit deriving (Show)
type Deck = [Card]

newtype Foundation = Foundation Deck
newtype Column =  Column Deck
newtype Reserve = Reserve Deck
newtype EOBoard = EOBoard Deck

pack :: Deck
pack = [Card pip suit | pip <- [Ace .. King], suit <- [Clubs .. Spades]]

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

