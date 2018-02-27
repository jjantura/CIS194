module Lib
    ( fib, 
    fibs1
    ) where

-- exercise 1
fib :: Integer -> Integer
fib n = if n == 0 || n == 1  then 1 else fib(n - 1) + fib(n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]