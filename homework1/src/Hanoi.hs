module Hanoi (
    hanoi,
    Peg,
    Move
    
) where 

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
