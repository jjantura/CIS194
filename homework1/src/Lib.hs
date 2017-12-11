module Lib
    ( toDigits,
    toDigitsRev,
    doubleEveryOther
    ) where

-- exercise 1
toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n
                
toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = [] | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10) 

-- exercise2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = undefined
