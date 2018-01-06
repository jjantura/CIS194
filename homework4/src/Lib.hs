module Lib
    ( fun1,
    fun1',
    fun2,
    fun2',
    foldTree
    ) where

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
foldTree xs = foldr (\val acc -> insert val acc) Leaf xs

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 (Leaf) x (Leaf)
insert x (Node n left val right) 
    | h1 < h2 = Node n (insert x left) val right
    | h1 > h2 = Node n left val insertRight
    | otherwise = Node (h+1) left val insertRight
    where
        h1 = getHeight left
        h2 = getHeight right
        insertRight = insert x right
        h = getHeight $ insertRight
            
getHeight :: Tree a -> Integer
getHeight Leaf = 0
getHeight (Node n left val right) = n