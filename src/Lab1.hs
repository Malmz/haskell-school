module Lab1 where

import Control.Exception
import Test.QuickCheck

{- Lab 1
   Authors: Hampus de Flon, Hannes Kaulio, Carl Malmgren

   Lab group: 16
 -}
---------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1 


-- B -------------------------
-- power1

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product (take (fromInteger k) (repeat n))

power1' :: Integer -> Integer -> Integer
power1' n k | k < 0 = error "power negative argument"
power1' n k = product [n | x <- [1..k]]


-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power negative argument"
power2 n 0 = 1
power2 n k
   | even k = power2 (n*n) (k `div` 2)
   | odd k = n * power2 n (k - 1)
   
-- power2 4 3 = 4 * (16 * 1)

-- D -------------------------
{- 


<Describe your test cases here>
We check some important edgecases
   x^0 = 1
   x^1 = x
   0^x = 0 if x > 0
   1^x = 1 if x >= 0
   
Then assuming that `power` is a correct implementation, we compare the output of `power1` and `power2` to `power` for all combinations of n in [-100, 100] and k in [0, 100]
None of the functions are implemented for k < 0 and therefor we don't test for that. (We would like to test that k < 0 gives an exception but can't figure that out)
 -}
 


-- comparePower1
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = power n k == power1 n k


-- comparePower2
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = power n k == power2 n k

-- Test functions: 

-- |Test if power functions return the expected value
testEq :: Integer -> Integer -> Integer -> Bool
testEq n k r = power n k == r && comparePower1 n k && comparePower2 n k

-- |Test edge cases
compareEdge :: Integer -> Bool
compareEdge x = 
   testEq x 0 1 && 
   testEq x 1 x && 
   if x >= 0 then 
      testEq 1 x 1 
   else True &&
   if x > 0 then
      testEq 0 x 0
   else True

compareAll :: Bool
compareAll = and [(comparePower1 x y) && (comparePower2 x y) && compareEdge x | x <- [-10..10], y <- [0..10]]


table :: Integer -> Integer -> IO ()
table n k = putStr (header ++ (unlines [line k1 (power n k1) (power1 n k1) (power2 n k1) | k1 <- [0..k]]))
   where line k p0 p1 p2 = show k ++ "\t" ++ show p0 ++ "\t" ++ show p1 ++ "\t" ++ show p2
         header = "k\tpower\tpower1\tpower2\n"