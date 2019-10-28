module Main where

import Test.QuickCheck

divides :: Integral a => a -> a -> Bool
m `divides` n = n `mod` m == 0

isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
isPrime 2 = True
isPrime n = not $ any (`divides` n) [2..n-1]

main :: IO ()
main = quickCheck $ \n -> isPrime n ==> isPrime (2^n - 1)
