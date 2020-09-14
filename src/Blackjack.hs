module Blackjack where

import Cards
import RunGame

-- Cards and Hands

aCard1 :: Card
aCard1 = Card (Numeric 2) Hearts -- define your favorite card here

aCard2 :: Card
aCard2 = Card Ace Diamonds 


aHand :: Hand 
aHand = [aCard1,aCard2, Card Ace Hearts] -- a Hand with two Cards, aCard1 and aCard2

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
sizeSteps = [
    size aHand2,
    size (Card (Numeric 2) Hearts : (Card Jack Spades : [])),
    1 + size (Card Jack Spades : []),
    1 + 1 + size [],
    1 + 1 + 0,
    2
    ]


-- Task A2

-- |Displays in format "number" of "Suit" (simply adds of)
displayCard :: Card -> String
displayCard (Card (Numeric i) s) = show i ++ " of " ++ show s
displayCard (Card r s) = show r ++ " of " ++ show s

-- |Displays the cards in type string.
display :: Hand -> String
display h = unlines $ map displayCard h


-- Task A3

-- |takes a rank and gives the numeric value
valueRank :: Rank -> Int
valueRank (Numeric i) = i
valueRank Ace = 11 
valueRank x = 10

-- |gives the value of a card
valueCard :: Card -> Int
valueCard (Card x s) = valueRank x

numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (x:xs) = (if valueCard x == 11 then 1 else 0) + numberOfAces xs

-- |decreases the value of Ace(s) to 1 incase value>21 otherwise it calculates the total value.
value :: Hand -> Int 
value h = if total > 21 then total - 10 * numberOfAces h else total
    where total = sum $ map valueCard h


-- Task A4

-- |gameover is true if value exceeds 21, else false.
gameOver :: Hand -> Bool
gameOver x = value x > 21 

-- |checks if the value exceeds 21 and then makes the opposite player a winner. Also checks the greatest value between Guest and Bank.
winner :: Hand -> Hand -> Player 
winner guest bank 
    | gameOver guest = Bank
    | gameOver bank = Guest 
    | value bank >= value guest = Bank
    | otherwise = Guest


prop_value = value aHand == 4 && value aHand2 == 12


allRanks :: [Rank]
allRanks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

fullDeck :: Deck
fullDeck = [Card r s | r <- allRanks, s <- allSuits]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

draw :: Deck -> Hand -> (Deck, Hand)
draw [] _     = error"draw: the deck is empty"
--draw d h      = (tail d,(head d):h)
draw (d:ds) h = (ds, d:h)



--prop_drawUntil = drawUntil 16 fullDeck [] == 18

playBank :: Deck -> Hand
playBank d = playBank' d [] --drawUntil 16 d []

playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand 
    | value bankHand > 16 = bankHand
    | otherwise = playBank' deck' bankHand' 
    where (deck', bankHand') = draw deck bankHand