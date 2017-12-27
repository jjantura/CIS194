module Golf
    ( 
        skips,
        localMaxima,
        histogram
    ) where

-- exercise 1
-- picks mod n elements n = 1 - every, n = 2 even and so on
picks n xs = foldr (\e a -> if (fst e `mod` n) == 0 then snd e:a else a) [] (zip [1..] xs)

skips :: [a] -> [[a]]
skips xs = foldr (\e a -> picks e xs:a) [] [1..length xs]

-- exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima n = [ n !! i | i <-[1..length n - 2], n !! i > n !! (i - 1) && n !! i > n !! (i + 1)]

-- exercise 3
histogram :: [Integer] -> String
histogram = "\n==========\n0123456789\n""

