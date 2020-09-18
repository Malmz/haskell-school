module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Cards and Hands

aCard1 :: Card
aCard1 = Card (Numeric 2) Hearts -- define your favorite card here

aCard2 :: Card
aCard2 = Card Ace Diamonds

aHand :: Hand
aHand = [aCard1, aCard2, Card Ace Hearts] -- a Hand with two Cards, aCard1 and aCard2

aHand2 :: Hand
aHand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

-- Task A1

{-
    size hand2
    = size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
    = 1 + size (Card Jack Spades : [])
    = 1 + 1 + size []
    = 1 + 1 + 0
    = 2
 -}

sizeSteps :: [Int]
sizeSteps =
  [ size aHand2,
    size (Card (Numeric 2) Hearts : (Card Jack Spades : [])),
    1 + size (Card Jack Spades : []),
    1 + 1 + size [],
    1 + 1 + 0,
    2
  ]

-- Task A2

-- | Displays a card with the format "Rank of Suit"
displayCard :: Card -> String
displayCard (Card (Numeric i) s) = show i ++ " of " ++ show s
displayCard (Card r s) = show r ++ " of " ++ show s

-- | Displays a hand
display :: Hand -> String
display h = unlines $ map displayCard h

-- Task A3

-- | Gives the value of a rank
valueRank :: Rank -> Int
valueRank (Numeric i) = i
valueRank Ace = 11
valueRank x = 10

-- | Gives the value of a card
valueCard :: Card -> Int
valueCard (Card x s) = valueRank x

numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (x : xs) = (if valueCard x == 11 then 1 else 0) + numberOfAces xs

-- | Calculates the value of a hand
value :: Hand -> Int
value h = if total > 21 then total - 10 * numberOfAces h else total
  where
    total = sum $ map valueCard h

-- Task A4

-- | Check if a hand is bust
gameOver :: Hand -> Bool
gameOver x = value x > 21

-- | Gives the winner of two hands
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest = Bank
  | gameOver bank = Guest
  | value bank >= value guest = Bank
  | otherwise = Guest

prop_value = value aHand == 4 && value aHand2 == 12

-- Task B1

allRanks :: [Rank]
allRanks = [Numeric n | n <- [2 .. 10]] ++ [Jack, Queen, King, Ace]

allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

fullDeck :: Deck
fullDeck = [Card r s | r <- allRanks, s <- allSuits]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2

-- | Draws top card from the deck and puts it on the hand
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "draw: the deck is empty"
--draw d h      = (tail d,(head d):h)
draw (d : ds) h = (ds, d : h)

-- Task B3

-- | Bank draws it's cards
playBank :: Deck -> Hand
playBank d = playBank' d []

playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand
  | value bankHand >= 16 = bankHand
  | otherwise = playBank' deck' bankHand'
  where
    (deck', bankHand') = draw deck bankHand

-- Task B4

-- | Shuffle a deck
shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle (r : rs) d = head s : shuffle rs (f ++ drop 1 s)
  where
    (f, s) = splitAt (floor (r * fromIntegral (length d))) d

--Task B5

-- | Check if card exist in deck
belongsTo :: Card -> Deck -> Bool
belongsTo _ [] = False
belongsTo c (c' : cs) = c == c' || c `belongsTo` cs

-- | Ensures that no cards get removed when shuffleing
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
  card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

-- | Ensures that the size of the deck is the same after the deck is shuffled
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = size deck == size (shuffle randomlist deck)

--Task B6
implementation :: Interface
implementation =
  Interface
    { iFullDeck = fullDeck,
      iValue = value,
      iDisplay = display,
      iGameOver = gameOver,
      iWinner = winner,
      iDraw = draw,
      iPlayBank = playBank,
      iShuffle = shuffle
    }
