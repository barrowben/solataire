{-
solataire.hs

8-off solataire game
https://en.wikipedia.org/wiki/Eight_Off

NOTE: For the columns, the last card in the list represents the bottommost card
i.e. the ones which can potentially be moved. Any card at the start of the list
cannot be moved until its successor are moved.

Author: Ben Barrow
Date: 03.11.2021
-}

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
type Stock = [Card]

data Board = EOBoard [Foundation] [Column] Reserve

instance Show Board where
    show (EOBoard f cs r) =
        "\nEOBoard\n" ++
        "Foundations   " ++ show f ++
        "\nColumns\n" ++
        "  " ++ show (head cs) ++ "\n" ++
        "  " ++ show (cs!!1) ++ "\n" ++
        "  " ++ show (cs!!2) ++ "\n" ++
        "  " ++ show (cs!!3) ++ "\n" ++
        "  " ++ show (cs!!4) ++ "\n" ++
        "  " ++ show (cs!!5) ++ "\n" ++
        "  " ++ show (cs!!6) ++ "\n" ++
        "  " ++ show (cs!!7) ++ "\n" ++
        "Reserve   " ++ show r ++ "\n"

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
shuffle n xs = [ x | (x, n) <- sortBy cmp (zip xs (randoms (mkStdGen n) :: [Int]))]

-- Generate 8 columns of 6 cards
startCol :: Deck -> [Column]
startCol d = [take 6 d,
                take 6 (drop 6 d),
                take 6 (drop 12 d),
                take 6 (drop 18 d),
                take 6 (drop 24 d),
                take 6 (drop 30 d),
                take 6 (drop 36 d),
                take 6 (drop 42 d)]

-- Initial board setup
eODeal :: Int -> Board
eODeal rand = EOBoard foundations columns reserve
    where
        shuffled = shuffle rand pack
        foundations  = [[],[],[],[]]
        columns = startCol shuffled
        reserve = [shuffled!!48, shuffled!!49, shuffled!!50, shuffled!!51]

-- Checks suit and returns an Int indicating to which Foundation pile it belongs
checkSuit :: Card -> Int
checkSuit (Card _ s)
    | s == Clubs = 0
    | s == Diamonds = 1
    | s == Hearts = 2
    | otherwise = 3

-- Add any card to correct pile in Foundation
-- TODO: BIT FUNKY... REFACTOR?
addCardFnd :: [Foundation] -> Card -> [Foundation]
addCardFnd f c
    | checkSuit c == 0 = [c]:tail f -- Clubs
    | checkSuit c == 1 = head f:[c]:drop 2 f -- Diamonds
    | checkSuit c == 2 = head f:f!!1:[c]:drop 3 f -- Hearts
    | otherwise = head f:f!!1:f!!2:[c]:drop 4 f -- Spades

-- Creates list of all cards on bottom of columns
getBtmColCards :: [Column] -> [Card]
getBtmColCards = map last

-- Check bottom card in columns for passed in card and remove if present
-- [DONE] Refactor using getBtmCards
-- TODO: REMOVE WHEN VERIFIED NEW FUNCTION IS WORKING CORRECTLY
-- removeFromCol :: [Column] -> Card -> [Column]
-- removeFromCol col card
--     | card == last (head col) = head (init col):tail col
--     | card == last (col!!1) = head col:init (col!!1):drop 2 col
--     | card == last (col!!2) = head col:col!!1:init (col!!2):drop 3 col
--     | card == last (col!!3) = head col:col!!1:col!!2:init (col!!3):drop 4 col
--     | card == last (col!!4) = head col:col!!1:col!!2:col!!3:init (col!!4):drop 5 col
--     | card == last (col!!5) = head col:col!!1:col!!2:col!!3:col!!4:init (col!!5):drop 6 col
--     | card == last (col!!6) = head col:col!!1:col!!2:col!!3:col!!4:col!!5:init (col!!6):drop 7 col
--     | card == last (col!!7) = head col:col!!1:col!!2:col!!3:col!!4:col!!5:col!!6:init (col!!7):drop 8 col -- This bit at the end is black magic, ask someone to explain why it behaves this this
--     | otherwise = col

removeFromCol :: [Column] -> Card -> [Column]
removeFromCol col card 
    | card `elem` getBtmColCards col = map (delete card) col
    | otherwise = col

removeFromReserve :: Reserve -> Card -> Reserve
removeFromReserve res card
    | card `elem` res = filter (/=card) res
    | otherwise = res

-- Moves all legal cards to Foundations
toFoundations :: Board -> Board
toFoundations (EOBoard f c r)
    | clubsuc `elem` r || clubsuc `elem` getBtmColCards c =
        toFoundations (EOBoard (addCardFnd f clubsuc) (removeFromCol c clubsuc) (removeFromReserve r clubsuc))
    | diamondsuc `elem` r || diamondsuc `elem` getBtmColCards c =
        toFoundations (EOBoard (addCardFnd f diamondsuc) (removeFromCol c diamondsuc) (removeFromReserve r diamondsuc))
    | heartsuc `elem` r || heartsuc `elem` getBtmColCards c =
        toFoundations (EOBoard (addCardFnd f heartsuc) (removeFromCol c heartsuc) (removeFromReserve r heartsuc))
    | spadesuc `elem` r || spadesuc `elem` getBtmColCards c =
        toFoundations (EOBoard (addCardFnd f spadesuc) (removeFromCol c spadesuc) (removeFromReserve r spadesuc))
    | otherwise = EOBoard f c r
        where
            clubsuc = if null (head f) then Card Ace Clubs else sCard (last (head f))
            diamondsuc = if null (f!!1) then Card Ace Diamonds else sCard (last (f!!1))
            heartsuc = if null (f!!2) then Card Ace Hearts else sCard (last (f!!2))
            spadesuc = if null (f!!3) then Card Ace Spades else sCard (last (f!!3))

-- Will be used later (Stage 2) to determine if stacks of card can be moved
getFreeReserveCount :: Reserve -> Int
getFreeReserveCount r = 8 - length r