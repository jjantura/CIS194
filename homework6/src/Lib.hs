module Lib
    ( fib, 
    fib2, 
    fibs1,
    fibs2
    ) where

-- exercise 1
fib :: Integer -> Integer
fib n = if n == 0 || n == 1  then 1 else fib(n - 1) + fib(n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2

fib2 :: Integer -> Integer -> [Integer]
fib2 x y = x : fib2 y (x + y)

fibs2 :: [Integer]
fibs2 = fib2 1 1

