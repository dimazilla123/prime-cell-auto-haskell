module Main (
    main
) where

import Rule
import Ring
import Universe

inputCicle :: Universe -> IO ()
inputCicle u = do
    inp <- getLine
    print u
    inputCicle . update $ u

main :: IO ()
main = do
    putStr "Enter rule num: "
    ruleStr <- getLine
    let rule = read ruleStr :: Int
    let u = Univ (Ring (True:replicate 10 False)) rule
    inputCicle u
