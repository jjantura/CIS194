module Lib
    ( eval,
    evalStr
    ) where

import Data.Maybe
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = if isJust $ maybeExp then Just $ eval (fromJust maybeExp) else Nothing 
            where 
                maybeExp = parseExp Lit Add Mul s
