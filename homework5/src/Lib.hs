
{-# OPTIONS_GHC -Wall -Werror #-}
-- {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Lib
    ( eval,
    evalStr,
    lit, mul, add
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
    lit a = if a <= 0 then True else False
    add a b = a || b
    mul a b = a && b
