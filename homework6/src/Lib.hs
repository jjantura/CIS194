{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( fib, 
    fib2, 
    fibs1,
    fibs2,
    Stream(..),
    streamToList
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

-- exercise 3
data Stream a = Cons a (Stream a) 

streamToList :: Stream a -> [a]
streamToList (Cons y z) = y:streamToList z

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList 
