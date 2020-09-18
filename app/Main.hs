module Main where

import Lib
import Blackjack
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

main :: IO ()
main = runGame implementation
