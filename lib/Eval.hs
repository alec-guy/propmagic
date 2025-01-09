module Eval where 

import Parser 

eval :: Proposition -> Bool 
eval prop = 
   case prop of 
    Boolean c -> 
        if c == 'T' then True else False 
    Not p1    -> not $ eval p1
    And p1 p2 -> (eval p1) && (eval p2)
    Or  p1 p2 -> (eval p1) || (eval p2)
    If  p1 p2 -> not $ (eval p1) && (not $ eval p2)
    Iff p1 p2 -> (eval p1) == (eval p2)
    Xor p1 p2 -> (eval p1) /= (eval p2)
    Nor p1 p2 -> (not $ eval p1) && (not $ eval p2)
    Nand p1 p2 -> not $ (eval p1) && (eval p2)
