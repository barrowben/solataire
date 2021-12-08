{-
solataire.hs

8-off solataire game
https://en.wikipedia.org/wiki/Eight_Off

NOTE: For the columns, the last card in the list represents the bottom-most card
i.e. the ones which can potentially be moved. Any card at the start of the list
cannot be moved until its successor are moved.

+ [DONE]end of list as "top card"? => Not a problem but probably best to refactor so that the whole list is not travesrsed to access card
+ [DONE]What is meant by "constant of board in appendix" => Hardcode it :(((
+ [DONE] type vs data? => type synonmn is fine
+ [TODO] reduce [Foundations] to Foundations i.e. single list and do not store any other card but top card

Author: Ben Barrow
Credit: Emma Norling for Shuffle function, taken and slightly modified from lecture slides
Date: 04.11.2021
-}

import System.Random
import Data.List
import Data.Ord
import Data.Maybe
import Debug.Trace
import Data.String (String)

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
             SBoard [Foundation] [Column] Stock deriving (Eq)

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

-- Get predecessor card
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
flipCard card = card {faceup = False}

-- Checks suit and returns an Int indicating to which Foundation pile it belongs
checkSuit :: Card -> Int
checkSuit (Card _ s _)
    | s == Clubs = 0
    | s == Diamonds = 1
    | s == Hearts = 2
    | otherwise = 3

-- Creates list of all cards on top of columns
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

-- Add cards to columns
addCardCol :: Column -> Card -> Column
addCardCol col card
    | sCard (head col) == card = card:col
    | otherwise = col

-- Remove card from reserve
removeFromReserve :: Reserve -> Card -> Reserve
removeFromReserve res card
    | card `elem` res = filter (/=card) res
    | otherwise = res

addCardRes :: Reserve -> Card -> Reserve
addCardRes r c = r++[c]

-- Moves all legal cards to Foundations for EO Solitaire
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

-- Return number of free spaces in Reserve
getFreeReserveCount :: Reserve -> Int
getFreeReserveCount r = 8 - length r

getColToResBoards :: Board -> [Board]
getColToResBoards (EOBoard f c r) = if getFreeReserveCount r > 0 then columnsToReserve (EOBoard f c r) topCards else []
    where topCards = getTopColCards c

-- Return all possible Boards moving top column card to reserve
columnsToReserve :: Board -> [Card] -> [Board]
columnsToReserve (EOBoard found col res) [] = []
columnsToReserve (EOBoard found col res) (c:cs)
    | getFreeReserveCount res > 0 = EOBoard found (removeFromCol col c) (addCardRes res c) : columnsToReserve (EOBoard found col res) cs
    | otherwise = columnsToReserve (EOBoard found col res) cs

-- moveResToCol :: Board -> [Board]
-- moveResToCol (EOBoard found col res) = [EOBoard found c r | c <- f, r <- g]
--     where f = map (`moveToCol` col) res
--           g = map (\y -> removeFromRes' y col res) res
--           topcards = getTopColCards col

getResColBoard :: Board -> [Board]
getResColBoard board = moveResToCol' board columnsunpack reserveunpack
    where zipped = zipResCol board
          columnsunpack = map snd zipped
          reserveunpack = map fst zipped

moveResToCol' :: Board -> [[Column]] -> [Reserve] -> [Board]
moveResToCol' (EOBoard found col res) [] (r:rs) = []
moveResToCol' (EOBoard found col res) (c:cs) [] = []
moveResToCol' (EOBoard found col res) [] [] = []
moveResToCol' (EOBoard found col res) (c:cs) (r:rs) = EOBoard found c r :  moveResToCol' (EOBoard found col res) cs rs

zipResCol :: Board -> [(Reserve, [Column])]
zipResCol (EOBoard found col res) = zip (map (\y -> removeFromRes' y col res) res) (map (`moveToCol` col) res)

moveToCol :: Card -> [Column] -> [Column]
moveToCol card [] = []
moveToCol card cols = map (\x -> (if (pCard (head x) == card) then (card : x) else x)) cols

removeFromRes' :: Card -> [Column] -> Reserve -> Reserve
removeFromRes' card col res = if sCard card `elem` topcard then filter (/= card) res else res
    where topcard = getTopColCards col

getAllMoveableCardsCol :: Column -> [Card]
getAllMoveableCardsCol [] = []
getAllMoveableCardsCol [f] = [f]
getAllMoveableCardsCol (f:s:cs)
    | isKing f = [f]
    | sCard f == s = f : getAllMoveableCardsCol (s:cs)
    | otherwise = [f]

removeFromCol' :: [Column] -> [Card] -> [Column]
removeFromCol' col cards = map (\\ cards) col

addCardAnyCol :: [Column] -> [Card] -> [Column]
addCardAnyCol col cards = map (\x -> if head x == sCard (last cards) then cards++x else x) (map (\\cards) (filter (not.null) col))

createColBoard :: [Card] -> Board -> Board
createColBoard cards (EOBoard f c r) = EOBoard f c2 r
    where c2 = addCardAnyCol c cards

neededCards :: [Column] -> [Card]
neededCards [] = []
neededCards col = map (pCard.head) (filter (not.null) col)

filterMoveableCard :: [Column] -> [Column]
filterMoveableCard col = filter (\x -> last x `elem` (neededCards col)) (map getAllMoveableCardsCol (filter (not.null) col))

findMoves :: Board -> [Board]
findMoves board@(EOBoard f c r) = nub (map toFoundations (list ++ getResColBoard board  ++ getColToResBoards board))
                                                where list = map (\x -> createColBoard x board) (filterMoveableCard c)

chooseMove :: Board -> Maybe Board
chooseMove board@(EOBoard f c r)
    | null (findMoves board) = Nothing
    | board == head (findMoves board) = Nothing
    | otherwise = Just (head (findMoves board))

haveWon :: Board -> Bool
haveWon (EOBoard f c r) = sum (map length f) == 52

getScore :: Board -> Int
getScore (EOBoard f c r) = sum (map length f)

playSolitaire :: Board -> Int
playSolitaire  board@(EOBoard f c r) = getScore(recurseChooseMove board)

recurseChooseMove :: Board -> Board
recurseChooseMove board@(EOBoard f c r)
    | isJust (chooseMove board) = recurseChooseMove ((fromJust.chooseMove) board)
    | otherwise = board

-- i = seed, j = number games
analyseEO :: Int -> Int -> (Int, Int)
analyseEO i j = (wonCount, average)
    where num = take j (randoms (mkStdGen i) :: [Int])
          score = sum (map (playSolitaire.eODeal) num)
          wonCount = length (filter (==52) (map (playSolitaire.eODeal) num))
          average = score `div` j

{- Paste the contents of this file, including this comment, into your source file, below all
     of your code. You can change the indentation to align with your own, but other than this,
     ONLY make changes as instructed in the comments.
   -}
  -- Constants that YOU must set:
studentName = "Benedict Barrow"
studentNumber = "200176657"
studentUsername = "aca20bab"

initialBoardDefined = screenshotBoard {- replace XXX with the name of the constant that you defined
                            in step 3 of part 1 -}
secondBoardDefined = screenshotBoard {- replace YYY with the constant defined in step 5 of part 1,
                            or if you have chosen to demonstrate play in a different game
                            of solitaire for part 2, a suitable contstant that will show
                            your play to good effect for that game -}

{- Beyond this point, the ONLY change you should make is to change the comments so that the
    work you have completed is tested. DO NOT change anything other than comments (and indentation
    if needed). The comments in the template file are set up so that only the constant eight-off
    board from part 1 and the toFoundations function from part 1 are tested. You will probably
    want more than this tested.

    CHECK with Emma or one of the demonstrators if you are unsure how to change this.

    If you mess this up, your code will not compile, which will lead to being awarded 0 marks
    for functionality and style.
-}

main :: IO()
main =
    do
    putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

    putStrLn "***The eight-off initial board constant from part 1:"
    print initialBoardDefined

    let board = toFoundations initialBoardDefined
    putStrLn "***The result of calling toFoundations on that board:"
    print board

    {- Move the start comment marker below to the appropriate position.
    If you have completed ALL the tasks for the assignment, you can
    remove the comments from the main function entirely.
    DO NOT try to submit/run non-functional code - you will receive 0 marks
    for ALL your code if you do, even if *some* of your code is correct.
    -}

    -- start comment marker - move this if appropriate

    let boards = findMoves board      -- show that findMoves is working
    putStrLn "***The possible next moves after that:"
    print boards

    let chosen = chooseMove board     -- show that chooseMove is working
    putStrLn "***The chosen move from that set:"
    print chosen

    putStrLn "***Now showing a full game"     -- display a full game
    score <- displayGame initialBoardDefined 0
    putStrLn $ "Score: " ++ score
    putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


    putStrLn "\n\n\n************\nNow looking at the alternative game:"

    putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
    print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                    -- is not an initial game, but a point from which the game
                                    -- can be won

    putStrLn "***Now showing a full game for alternative solitaire"
    score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                            -- works correctly)
    putStrLn $ "Score: " ++ score
    putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)



{- displayGame takes a Board and move number (should initially be 0) and
    displays the game step-by-step (board-by-board). The result *should* be
    the same as performing playSolitaire on the initial board, if it has been
    implemented correctly.
    DO NOT CHANGE THIS CODE other than aligning indentation with your own.
-}
displayGame :: Board -> Int ->IO String
displayGame board n =
    if haveWon board
        then return "A WIN"
        else
        do
            putStr ("Move " ++ show n ++ ": " ++ show board)
            let maybeBoard = chooseMove board
            if isJust maybeBoard then
                do
                    let (Just newBoard) = maybeBoard
                    displayGame newBoard (n+1)
                else
                do
                    let score = show (playSolitaire board)
                    return score
