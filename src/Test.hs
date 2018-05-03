module Main 
where

import Rule
import Data.Bits

if' :: Bool -> a -> a -> a
if' True a b = a
if' False a b = b

main :: IO ()
main = do
    inp <- getLine
    let x = read inp :: Int
        states = [(False, False, False),
                  (False, False, True),
                  (False, True, False),
                  (False, True, True),
                  (True, False, False),
                  (True, False, True),
                  (True, True, False),
                  (True, True, True)] in
        print . fmap (\a -> (getStateN a `shiftR` x) `mod` 2) $ states
