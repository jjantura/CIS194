module Lib
    ( fun1,
    fun1',
    fun2,
    fun2',
    foldTree,
    xor,
    map',
    sieveSundaram
    ) where

import Data.List

-- exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) | even x = (x - 2) * fun1 xs | otherwise = fun1 xs

fun1' = product . map(\x -> x - 2) . filter even

-- sum of all even numbers in collatz sequence
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2) | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ filter even $ takeWhile (/=1) $ iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n

-- exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)
foldTree :: [a] -> Tree a
foldTree = foldr ins Leaf

ins :: a -> Tree a -> Tree a
ins x Leaf = Node 0 Leaf x Leaf
ins x (Node n left val right) 
    | h1 < h2 = Node n (ins x left) val right
    | h1 > h2 = Node n left val insertRight
    | otherwise = Node (h+1) left val insertRight
    where
        h1 = getHeight left
        h2 = getHeight right
        insertRight = ins x right
        h = getHeight $ insertRight
            
getHeight :: Tree a -> Integer
getHeight Leaf = 0
getHeight (Node n left val right) = n

-- exercise 3
-- 3.1
xor :: [Bool] -> Bool
xor xs = odd $ foldl (\a e -> if e then a + 1 else a) 0 xs

-- 3.2
map' :: (a->b) -> [a] -> [b]
map' f = foldr (\e a -> [f e] ++ a) [] 


-- exercise 4

toBeFiltered :: Integer -> [Integer]
toBeFiltered n = takeWhile (<=n) [ i + j + 2 * i * j | i <- [1..], j <- [i..]]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (+1) $ map (*2) $ [1..n] \\ toBeFiltered n
