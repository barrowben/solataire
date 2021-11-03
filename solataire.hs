{-
solataire.hs

8-off solataire game
https://en.wikipedia.org/wiki/Eight_Off

Author: Ben Barrow
Date: 03.11.2021
-}

import System.Random
import Data.List
import GHC.Types (Bool(True))

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

isClubs :: Card -> Bool
isClubs (Card _ s) | s == Clubs = True
                   | otherwise = False

isDiamonds :: Card -> Bool
isDiamonds (Card _ s) | s == Diamonds = True
                   | otherwise = False

isHearts :: Card -> Bool
isHearts (Card _ s) | s == Hearts = True
                   | otherwise = False

isSpades :: Card -> Bool
isSpades (Card _ s) | s == Spades = True
                   | otherwise = False

checkSuit :: Card -> Int
checkSuit (Card _ s)
    | s == Clubs = 0
    | s == Diamonds = 1
    | s == Hearts = 2
    | otherwise = 3

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

-- Move Aces from Reserve to Foundations
moveResAcesFoundations :: Board -> Board
moveResAcesFoundations (EOBoard f c r)
    | ac `elem` r = moveResAcesFoundations (EOBoard (addAnyFound f ac) c (filter (/=ac) r))
    | ad `elem` r = moveResAcesFoundations (EOBoard (addAnyFound f ad) c (filter (/=ad) r))
    | ah `elem` r = moveResAcesFoundations (EOBoard (addAnyFound f ah) c (filter (/=ah) r))
    | as `elem` r = moveResAcesFoundations (EOBoard (addAnyFound f as) c (filter (/=as) r))
    | otherwise = EOBoard f c r
        where
            ac = Card Ace Clubs
            ad = Card Ace Diamonds
            ah = Card Ace Hearts
            as = Card Ace Spades

-- Move Aces from Column to Foundations
moveColAcesFoundations :: Board -> Board
moveColAcesFoundations (EOBoard f c r)
  | isAce (last (head c)) = moveColAcesFoundations (EOBoard (addAnyFound f (last (head c))) (head (init c):tail c) r)
  | isAce (last (c!!1)) = moveColAcesFoundations (EOBoard (addAnyFound f (last (c!!1))) (removeFromColumns c (last (c!!1))) r)
  | isAce (last (c!!2)) = moveColAcesFoundations (EOBoard (addAnyFound f (last (c!!2))) (removeFromColumns c (last (c!!2))) r)
  | isAce (last (c!!3)) = moveColAcesFoundations (EOBoard (addAnyFound f (last (c!!3))) (removeFromColumns c (last (c!!3))) r)
  | isAce (last (c!!4)) = moveColAcesFoundations (EOBoard (addAnyFound f (last (c!!4))) (removeFromColumns c (last (c!!4))) r)
  | isAce (last (c!!5)) = moveColAcesFoundations (EOBoard (addAnyFound f (last (c!!5))) (removeFromColumns c (last (c!!5))) r)
  | isAce (last (c!!6)) = moveColAcesFoundations (EOBoard (addAnyFound f (last (c!!6))) (removeFromColumns c (last (c!!6))) r)
  | isAce (last (c!!7)) = moveColAcesFoundations (EOBoard (addAnyFound f (last (c!!7))) (removeFromColumns c (last (c!!7))) r)
  | otherwise = EOBoard f c r

-- Add any card to correct pile in Foundation
addAnyFound :: [Foundation] -> Card -> [Foundation]
addAnyFound f c
    | checkSuit c == 0 = [c]:tail f -- Clubs
    | checkSuit c == 1 = head f:[c]:drop 2 f -- Diamonds
    | checkSuit c == 2 = head f:f!!1:[c]:drop 3 f -- Hearts
    | otherwise = head f:f!!1:f!!2:[c]:drop 4 f -- Spades

-- Check bottom card in columns for passed in card and remove if present
removeFromColumns :: [Column] -> Card -> [Column]
removeFromColumns col car
    | car == last (head col) = head (init col):tail col
    | car == last (col!!1) = head col:init (col!!1):drop 2 col
    | car == last (col!!2) = head col:col!!1:init (col!!2):drop 3 col
    | car == last (col!!3) = head col:col!!1:col!!2:init (col!!3):drop 4 col
    | car == last (col!!4) = head col:col!!1:col!!2:col!!3:init (col!!4):drop 5 col
    | car == last (col!!5) = head col:col!!1:col!!2:col!!3:col!!4:init (col!!5):drop 6 col
    | car == last (col!!6) = head col:col!!1:col!!2:col!!3:col!!4:col!!5:init (col!!6):drop 7 col
    | car == last (col!!7) = head col:col!!1:col!!2:col!!3:col!!4:col!!5:col!!6:init (col!!7):drop 8 col
    | otherwise = col

-- moveNonAcesFoundations :: Board -> Board
-- moveNonAcesFoundations (EOBoard f c r)
--     | null l = moveResAcesFoundations.moveColAcesFoundations
--     | c `elem` r = (EOBoard f c r)
--     | c `elem` c 
--         where
--             l = head f
--             c = getSucc (tail l)
--             s = getSuit c