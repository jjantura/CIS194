module Golf
    ( 
        skips,
        localMaxima,
        count,
        histogram,
        histogram2,
        maxHeight,
        scanline
    ) where

-- exercise 1
-- picks mod n elements n = 1 - every, n = 2 even and so on
picks n xs = foldr (\e a -> if (fst e `mod` n) == 0 then snd e:a else a) [] (zip [1..] xs)

skips :: [a] -> [[a]]
skips xs = foldr (\e a -> picks e xs:a) [] [1..length xs]

-- exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima n = [ n !! i | i <-[1..length n - 2], n !! i > n !! (i - 1) && n !! i > n !! (i + 1)]

-- exercise 3 - helper function, count number of occurences
count :: [Integer] -> Integer -> Int
count xs n = length $ filter(==n) xs 

-- helper functiom, calculate 'classic' histogram
histogram2 :: [Integer] -> [(Integer, Int)]
histogram2 xs = foldr (\e a -> (e, count xs e):a) [] [0..9] 

-- convert classic histogram to a line
scanline :: Integer -> [Integer] -> String
scanline n xs = foldl (\a e -> if e <= n then '*':a else ' ':a) [] xs

maxHeight :: [Integer] -> Int
maxHeight xs = maximum $ map (snd) $ histogram2 xs

-- exercise 3
histogram :: [Integer] -> String
histogram xs = "\n==========\n0123456789\n"

