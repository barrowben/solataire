{-
solataire.hs

8-off solataire game
https://en.wikipedia.org/wiki/Eight_Off

NOTE: For the columns, the last card in the list represents the bottom-most card
i.e. the ones which can potentially be moved. Any card at the start of the list
cannot be moved until its successor are moved.

+ [DONE]end of list as "top card"? => Not a problem but probably best to refactor so that the whole list is not travesrsed to access card
+ What is meant by following?   

A constant of type Board that shows the game in progress (that is, the state of the board at
that particular move) for the screenshot shown in Appendix B. => Hard code it :(

[DONE]+ type vs data? => type synonmn is fine

Author: Ben Barrow
Credit: Emma Norling for Shuffle function, taken and slightly modified from lecture slides
Date: 04.11.2021
-}

import System.Random
import Data.List

-- Define data structures
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
            deriving (Eq, Enum, Show, Bounded)
data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Enum, Show, Bounded)
data Card = Card {pip :: Pip, suit :: Suit, faceup :: Bool}
            deriving (Eq)

-- Output the card. If the card is facedown, <unknown> will be displayed
instance Show Card where
    show (Card p s u) = if u then "(" ++ show p ++ " of " ++ show s ++ ")" else "(<unknown>)"

type Deck = [Card]
type Foundation = [Card]
type Reserve = [Card]
type Column = [Card]
type Stock = [Card]

data Board = EOBoard [Foundation] [Column] Reserve |
             SBoard [Foundation] [Column] Stock

instance Show Board where
    show (EOBoard fs cs r) =
        "\nEOBoard\n" ++
        "Foundations   " ++ show fs ++
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

    show (SBoard fs cs r) =
        "\nSBoard\n" ++
        "Foundations   " ++ show fs ++
        "\nColumns\n" ++
        "  " ++ show (head cs) ++ "\n" ++
        "  " ++ show (cs!!1) ++ "\n" ++
        "  " ++ show (cs!!2) ++ "\n" ++
        "  " ++ show (cs!!3) ++ "\n" ++
        "  " ++ show (cs!!4) ++ "\n" ++
        "  " ++ show (cs!!5) ++ "\n" ++
        "  " ++ show (cs!!6) ++ "\n" ++
        "  " ++ show (cs!!7) ++ "\n" ++
        "  " ++ show (cs!!8) ++ "\n" ++
        "  " ++ show (cs!!9) ++ "\n" ++
        "Stock " ++ show (length r `div` 10) ++ " Deals Remaining "

-- The hardcoded Board from Appendix A
screenshotBoard :: Board
screenshotBoard = EOBoard fs cs rs
    where
        fs  = [[],[],[],[]]
        cs = [[Card Ace Clubs t, Card Seven Diamonds t, Card Ace Hearts t, Card Queen Hearts t, Card King Clubs t, Card Four Spades t],
             [Card Five Diamonds t, Card Queen Spades t, Card Three Diamonds t, Card Five Spades t, Card Six Spades t, Card Seven Hearts t],
             [Card King Hearts t, Card Ten Diamonds t, Card Seven Spades t, Card Queen Diamonds t, Card Five Hearts t, Card Eight Diamonds t],
             [Card Jack Spades t, Card Six Hearts t, Card Seven Clubs t, Card Eight Spades t, Card Ten Clubs t, Card Queen Clubs t],
             [Card Ace Spades t, Card Eight Clubs t, Card Ace Diamonds t, Card King Diamonds t, Card Jack Hearts t, Card Four Clubs t],
             [Card Two Diamonds t, Card Three Hearts t, Card Three Clubs t, Card Ten Hearts t, Card Six Diamonds t, Card Jack Clubs t],
             [Card Nine Spades t, Card Four Diamonds t, Card Nine Clubs t, Card Nine Hearts t, Card Three Spades t, Card Ten Spades t],
             [Card Two Clubs t, Card Two Spades t, Card Four Hearts t, Card Nine Diamonds t, Card King Spades t, Card Eight Hearts t]]
        rs = [Card Two Hearts True, Card Six Clubs True, Card Five Clubs True, Card Jack Diamonds True]
        t = True


-- Define a 52-card deck, cards face-up by default
pack :: Deck
pack = [Card p s u | p <- [minBound .. maxBound], s <- [minBound .. maxBound], u <- [True ..]]

-- Define a 104-card deck
sPack :: Deck
sPack = pack++pack

-- Get successor card
sCard :: Card -> Card
sCard (Card p s _)
    | p == King = Card Ace s True
    | otherwise = Card (succ p) s True

-- Get predecessor card [NOT USED]
pCard :: Card -> Card
pCard (Card p s _)
    | p == Ace = Card King s True
    | otherwise = Card (pred p) s True

-- Determine if Ace
isAce :: Card -> Bool
isAce (Card p _ _) | p == Ace = True
                 | otherwise = False

-- Determine if King [NOT USED]
isKing :: Card -> Bool
isKing (Card p _ _) | p == King = True
                  | otherwise = False

-- Shuffle deck of cards
cmp (x1, y1) (x2, y2) = compare y1 y2
shuffle :: Int -> [a] -> [a]
shuffle n xs = [ x | (x, n) <- sortBy cmp (zip xs (randoms (mkStdGen n) :: [Int]))]

-- Generate starting columns for eight-off
startCol :: Deck -> [Column]
startCol d = [take 6 d,
                take 6 (drop 6 d),
                take 6 (drop 12 d),
                take 6 (drop 18 d),
                take 6 (drop 24 d),
                take 6 (drop 30 d),
                take 6 (drop 36 d),
                take 6 (drop 42 d)]

-- Generate starting columns for spider
sStartCol :: Deck -> [Column]
sStartCol d = [take 6 d,
                take 6 (drop 6 d),
                take 6 (drop 12 d),
                take 6 (drop 18 d),
                take 5 (drop 24 d),
                take 5 (drop 29 d),
                take 5 (drop 34 d),
                take 5 (drop 49 d),
                take 5 (drop 44 d),
                take 5 (drop 49 d)]

-- Initial eight-off board setup
eODeal :: Int -> Board
eODeal rand = EOBoard foundations columns reserve
    where
        shuffled = shuffle rand pack
        foundations  = [[],[],[],[]]
        columns = startCol shuffled
        reserve = drop 48 shuffled

-- Initial spider board setup
sDeal :: Int -> Board
sDeal rand = hideAll (SBoard f c s)
    where
        shuffled = shuffle rand sPack
        f = [[],[],[],[],[],[],[],[]]
        c = sStartCol shuffled
        s = drop 54 shuffled

-- Functions to hide cards
hideAll :: Board -> Board
hideAll (SBoard f c s) = SBoard f (map hideCards c) s

hideCards :: Column -> Column
hideCards col = head col : map flipCard facedown
    where facedown = tail col

flipCard :: Card -> Card
flipCard card = card { faceup = False }

-- Checks suit and returns an Int indicating to which Foundation pile it belongs
checkSuit :: Card -> Int
checkSuit (Card _ s _)
    | s == Clubs = 0
    | s == Diamonds = 1
    | s == Hearts = 2
    | otherwise = 3

-- Creates list of all cards on bottom of columns
getTopColCards :: [Column] -> [Card]
getTopColCards = map head

-- Add any card to correct pile in Foundation
addCardFnd :: [Foundation] -> Card -> [Foundation]
addCardFnd f c
    | checkSuit c == 0 = [c]:tail f -- Clubs
    | checkSuit c == 1 = head f:[c]:drop 2 f -- Diamonds
    | checkSuit c == 2 = take 2 f++[c]:drop 3 f -- Hearts
    | otherwise = take 3 f++[c]:drop 4 f -- Spades

-- Remove card from column
removeFromCol :: [Column] -> Card -> [Column]
removeFromCol col card
    | card `elem` getTopColCards col = map (delete card) col
    | otherwise = col

-- Remove card from reserve
removeFromReserve :: Reserve -> Card -> Reserve
removeFromReserve res card
    | card `elem` res = filter (/=card) res
    | otherwise = res

-- Moves all legal cards to Foundations
toFoundations :: Board -> Board
toFoundations (EOBoard f c r)
    | clubsuc `elem` r = toFoundations (EOBoard (addCardFnd f clubsuc) c (removeFromReserve r clubsuc))
    | diamondsuc `elem` r  = toFoundations (EOBoard (addCardFnd f diamondsuc) c (removeFromReserve r diamondsuc))
    | heartsuc `elem` r = toFoundations (EOBoard (addCardFnd f heartsuc) c (removeFromReserve r heartsuc))
    | spadesuc `elem` r = toFoundations (EOBoard (addCardFnd f spadesuc) c (removeFromReserve r spadesuc))
    | clubsuc `elem` getTopColCards c = toFoundations (EOBoard (addCardFnd f clubsuc) (removeFromCol c clubsuc) r)
    | diamondsuc `elem` getTopColCards c = toFoundations (EOBoard (addCardFnd f diamondsuc) (removeFromCol c diamondsuc) r)
    | heartsuc `elem` getTopColCards c = toFoundations (EOBoard (addCardFnd f heartsuc) (removeFromCol c heartsuc) r)
    | spadesuc `elem` getTopColCards c = toFoundations (EOBoard (addCardFnd f spadesuc) (removeFromCol c spadesuc) r)
    | otherwise = EOBoard f c r
        where
            clubsuc = if null (head f) then Card Ace Clubs True else sCard (last (head f))
            diamondsuc = if null (f!!1) then Card Ace Diamonds True else sCard (last (f!!1))
            heartsuc = if null (f!!2) then Card Ace Hearts True else sCard (last (f!!2))
            spadesuc = if null (f!!3) then Card Ace Spades True else sCard (last (f!!3))

-- Will be used later (Stage 2) to determine if stacks of card can be moved
getFreeReserveCount :: Reserve -> Int
getFreeReserveCount r = 8 - length r