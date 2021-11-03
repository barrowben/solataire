import System.Random
import Data.List

-- Define data structures
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
            deriving (Eq, Enum, Show, Bounded)
data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Enum, Show, Bounded)
data Card = Card Pip Suit
            deriving (Eq)

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

-- Define a 52-card deck
pack :: Deck
pack = [Card pip suit | pip <- [Ace .. King], suit <- [Clubs .. Spades]]

-- Get successor card
sCard :: Card -> Card
sCard (Card p s)
    | p == King = Card Ace s
    | otherwise = Card (succ p) s

-- Get predecessor card
pCard :: Card -> Card
pCard (Card p s)
    | p == Ace = Card King s
    | otherwise = Card (pred p) s

-- Determine if Ace
isAce :: Card -> Bool
isAce (Card p _) | p == Ace = True
                 | otherwise = False

-- Determine if King
isKing :: Card -> Bool
isKing (Card p _) | p == King = True
                  | otherwise = False

-- Shuffle deck of cards
cmp (x1, y1) (x2, y2) = compare y1 y2
shuffle :: Int -> [a] -> [a]
shuffle n xs = [ x | (x, n) <- sortBy cmp
                          (zip xs (randoms (mkStdGen n) :: [Int]))]

-- Generate 8 columns of 6 cards
startColumns :: Deck -> [Column]
startColumns d = [take 6 d,
                 take 6 (drop 6 d),
                 take 6 (drop 12 d),
                 take 6 (drop 18 d),
                 take 6 (drop 24 d),
                 take 6 (drop 30 d),
                 take 6 (drop 36 d),
                 take 6 (drop 42 d)]

-- Take seed and generate random starting setup
eODeal :: Int -> Board
eODeal seed = EOBoard foundations columns reserve
    where
        shuffled = shuffle seed pack
        foundations  = [[],[],[],[]]
        columns = startColumns shuffled
        reserve = [shuffled!!48, shuffled!!49, shuffled!!50, shuffled!!51]

-- IMPLEMENT ONCE HELPER FUNCTIONS DONE
toFoundations :: Board -> Board
toFoundations = moveResAcesFoundations.moveColAcesFoundations

getFreeReserveCount :: Reserve -> Int
getFreeReserveCount r = 8 - length r

-- Move Aces from Reseve to Foundations
moveResAcesFoundations :: Board -> Board
moveResAcesFoundations (EOBoard f c r)
    | ac `elem` r = moveResAcesFoundations (EOBoard (addAcesToFoundation f ac) c (filter (/=ac) r))
    | ad `elem` r = moveResAcesFoundations (EOBoard (addAcesToFoundation f ad) c (filter (/=ad) r))
    | ah `elem` r = moveResAcesFoundations (EOBoard (addAcesToFoundation f ah) c (filter (/=ah) r))
    | as `elem` r = moveResAcesFoundations (EOBoard (addAcesToFoundation f as) c (filter (/=as) r))
    | otherwise = EOBoard f c r
        where
            ac = Card Ace Clubs
            ad = Card Ace Diamonds
            ah = Card Ace Hearts
            as = Card Ace Spades

moveColAcesFoundations :: Board -> Board
moveColAcesFoundations (EOBoard f c r)
  | isAce (last (head c)) = moveColAcesFoundations (EOBoard (addAcesToFoundation f (last (head c))) (head (init c):tail c) r)
  | isAce (last (c!!1)) = moveColAcesFoundations (EOBoard (addAcesToFoundation f (last (c!!1))) (head c:init (c!!1):drop 2 c) r)
  | isAce (last (c!!2)) = moveColAcesFoundations (EOBoard (addAcesToFoundation f (last (c!!2))) (head c:c!!1:init (c!!2):drop 3 c) r)
  | isAce (last (c!!3)) = moveColAcesFoundations (EOBoard (addAcesToFoundation f (last (c!!3))) (head c:c!!1:c!!2:init (c!!3):drop 4 c) r)
  | isAce (last (c!!4)) = moveColAcesFoundations (EOBoard (addAcesToFoundation f (last (c!!4))) (head c:c!!1:c!!2:c!!3:init (c!!4):drop 5 c) r)
  | isAce (last (c!!5)) = moveColAcesFoundations (EOBoard (addAcesToFoundation f (last (c!!5))) (head c:c!!1:c!!2:c!!3:c!!4:init (c!!5):drop 6 c) r)
  | isAce (last (c!!6)) = moveColAcesFoundations (EOBoard (addAcesToFoundation f (last (c!!6))) (head c:c!!1:c!!2:c!!3:c!!4:c!!5:init (c!!6):drop 7 c) r)
  | isAce (last (c!!7)) = moveColAcesFoundations (EOBoard (addAcesToFoundation f (last (c!!7))) (head c:c!!1:c!!2:c!!3:c!!4:c!!5:c!!6:init (c!!7):drop 8 c) r)
  | otherwise = EOBoard f c r

addAcesToFoundation :: [Foundation] -> Card -> [Foundation]
addAcesToFoundation f (Card Ace Clubs) = [Card Ace Clubs]:tail f
addAcesToFoundation f (Card Ace Diamonds) = head f:[Card Ace Diamonds]:drop 2 f
addAcesToFoundation f (Card Ace Hearts) = head f:f!!1:[Card Ace Hearts]:drop 3 f
addAcesToFoundation f (Card Ace Spades) = head f:f!!1:f!!2:[Card Ace Spades]:drop 4 f
addAcesToFoundation f (Card _ _) = f

