
{-# OPTIONS_GHC -Wall  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Lib
    ( eval,
    evalStr,
    lit, mul, add, MinMax, Mod7, compile
    ) where

import Data.Maybe
import ExprT
import Parser
import StackVM as StackVM

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    mul = ExprT.Mul
    add = ExprT.Add

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = if isJust $ maybeExp then Just $ eval (fromJust maybeExp) else Nothing 
            where 
                maybeExp = parseExp ExprT.Lit ExprT.Add ExprT.Mul s


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

-- exercise 5
instance Expr StackVM.Program where
    lit i = [StackVM.PushI i]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul    

