
{-# OPTIONS_GHC -Wall -Werror #-}
module Lib
    ( eval,
    evalStr,
    lit, mul, add, MinMax, Mod7
    ) where

import Data.Maybe
import ExprT
import Parser

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = if isJust $ maybeExp then Just $ eval (fromJust maybeExp) else Nothing 
            where 
                maybeExp = parseExp Lit Add Mul s


instance Expr Bool where
    lit a = a > 0
    add a b = a || b
    mul a b = a && b

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*) 

newtype MinMax = MinMax Integer deriving (Show, Eq)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b) 
    mul (MinMax a) (MinMax b) = MinMax (min a b) 

instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)     
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7) 
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)     