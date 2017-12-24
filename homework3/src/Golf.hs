module Golf
    ( 
        skips
    ) where

-- picks mod n elements n = 1 - every, n = 2 even and so on
picks n xs = foldr (\e a -> if (fst e `mod` n) == 0 then snd e:a else a) [] (zip [1..] xs)


skips :: [a] -> [[a]]
skips xs = foldr (\e a -> picks e xs:a) [] [1..length xs]
