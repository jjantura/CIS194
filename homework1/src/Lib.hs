module Lib
    ( toDigits,
    toDigitsRev,
    doubleEveryOther,
    sumDigits,
    validate
    ) where

-- exercise 1
toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n
                
toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = [] | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10) 

-- exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = foldl(\a e -> if fst e `mod` 2 == 0 then snd e:a else (2 * snd e) : a) [] (zip [0 .. (length xs) - 1] $ reverse xs) 

-- exercise 3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigitsRev ) xs

validate :: Integer -> Bool
validate xs = (sumDigits $ doubleEveryOther $ toDigits xs) `mod` 10 == 0 