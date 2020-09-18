module Exercise3 where

import Data.List
{-
take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n xs | n <= 0 = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take' n xs, drop' n xs)

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z):zip3' xs ys zs

isPermutation :: Eq a  => [a] -> [a] -> Bool
isPermutation xs ys = or [xs == ys' | ys' <- permutations ys]
 -}
import System.IO

-- |
-- Module      : Lecture3B
-- Description : Lecture week 3, part B, IO instructions
-- Copyright   : (c) 2020 TDA555/DIT440, Introduction to Functional Programming
-- License     : BSD
-- Maintainer  : alexg@chalmers.se
-- Stability   : experimental
--
-- Examples illustrating how to do Input/Output in Haskell.

--------------------------------------------------------------------------------

-- * Using the do notation for combining IO instructions

greeting :: IO ()
greeting = do
  putStr "Hej! Vad heter du?\n> "
  name <- getLine
  putStrLn ("Tjeeeena " ++ name ++ "!")

-- | Make a backup of a file
backup :: FilePath -> IO ()
backup filename = do
  txt <- readFile filename
  writeFile (filename ++ ".backup") txt

hello :: IO ()
hello = putStrLn "Hello!"

-- | Ask for two numbers and shows the difference
whatsTheDifference :: IO ()
whatsTheDifference = do
  putStrLn "Hej! What is the difference?"
  putStr "Give a number:\n> "
  x <- readLn
  putStr "Give a number:\n> "
  y <- readLn
  putStrLn ("Difference: " ++ show (x - y))

--------------------------------------------------------------------------------

-- * Creating IO instructions that return results

-- | Compute the number of lines in a file
lengthFile :: FilePath -> IO Int
lengthFile filename = do
  txt <- readFile filename
  return (length (lines txt))

--------------------------------------------------------------------------------

-- Example: use lengthFile to find out how many words there are
-- in the system dictionary

dict :: FilePath
dict = "/usr/share/dict/words" -- works on Unix-like system (macOS, Linux)

--------------------------------------------------------------------------------

-- * More modular version of whatsTheDifference

askForNumber :: IO Integer
askForNumber = do
  putStr "Give a number:\n> "
  readLn

getTheDifference :: IO Integer
getTheDifference = do
  x <- askForNumber
  y <- askForNumber
  return (x - y)

showTheDifference :: Integer -> IO ()
showTheDifference diff = print diff

whatsTheDifference_v2 :: IO ()
whatsTheDifference_v2 = do
  putStrLn "Hej! What is the difference?"
  diff <- getTheDifference
  showTheDifference diff

---------------------------------

greet :: IO ()
greet = do
  putStr "Hi whats your name?\n>"
  name <- getLine
  putStrLn ("Hello " ++ name ++ " :)")

main :: IO ()
main = do
  i <- getLine
  if i /= "quit"
    then do
      putStrLn ("Input: " ++ i)
      main
    else return ()

count :: Int -> Int -> IO ()
count n m = do
  putStrLn (show n)
  if n < m
    then count (n + 1) m
    else return ()
