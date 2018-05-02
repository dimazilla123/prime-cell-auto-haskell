module Rule 
where

import Data.Bits

type Rule = Int
type StateN = Int
type Cell = Bool

getStateN :: (Cell, Cell, Cell) -> StateN
getStateN (a, b, c) = let a1 = if a then 4; else 0
                          b1 = if b then 2; else 0
                          c1 = if c then 1; else 0
                          in
                            a1 + b1 + c1

applicateRule :: (Cell, Cell, Cell) -> Rule -> Cell
applicateRule (a, b, c) rule = let stateIndex = getStateN (a, b, c) in
                                   (rule `shiftR` stateIndex == 1)
